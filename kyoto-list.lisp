(in-package #:kyoto-persistence)

(defun print-klist (klist stream depth)
  (declare (ignore klist depth))
  (format stream "#<KLIST>"))

(defstruct (klist
             (:print-function print-klist)
             (:predicate klist?))
  db
  key
  (key-size 0)
  pointer 
  (size 0)
  (record 0)
  (offset 0)
  (items (make-array 0 :adjustable t :fill-pointer t)))

(defun element-position (seq seq-len elt elt-len)
  "Byte offset of the first element of serialized list SEQ (SEQ-LEN
bytes) whose bytes equal ELT's (ELT-LEN), or NIL.  Whole elements only:
a raw byte search matched inside other elements (kyoto-persistence#2)."
  (loop with offset = 0
        while (< offset seq-len)
        do (multiple-value-bind (data-length header-length)
               (extract-length seq offset)
             (let ((end (+ header-length data-length)))
               (when (and (= elt-len (- end offset))
                          (pointer-deref-eql (inc-pointer seq offset)
                                             elt elt-len))
                 (return offset))
               (setf offset end)))))

(defun remove-element (seq seq-len elt elt-len)
  "A kcmalloc'd copy of serialized list SEQ without its first element
equal to ELT, and its length.  SEQ and SEQ-LEN when ELT is absent; a
null pointer and 0 when ELT was the only element."
  (let ((pos (element-position seq seq-len elt elt-len)))
    (cond ((null pos) (values seq seq-len))
          ((= elt-len seq-len) (values (null-pointer) 0))
          (t (let* ((new-len (- seq-len elt-len))
                    (new (kcmalloc new-len)))
               (dotimes (i pos)
                 (setf (mem-aref new :unsigned-char i)
                       (mem-aref seq :unsigned-char i)))
               (loop for i from (+ pos elt-len) below seq-len
                     for j from pos
                     do (setf (mem-aref new :unsigned-char j)
                              (mem-aref seq :unsigned-char i)))
               (values new new-len))))))

(defmethod klist-free ((klist klist))
  (when (and (pointerp (klist-pointer klist)) (not (null-pointer-p (klist-pointer klist))))
    (kcfree (klist-pointer klist)))
  (when (and (pointerp (klist-key klist))
             (not (null-pointer-p (klist-key klist))))
    (kcfree (klist-key klist))))

(defmethod klist-remove ((klist klist) value
                         &optional (serializer #'serialize))
  (let (value-ptr value-size new-ptr new-len)
    (unwind-protect
         (progn
           (multiple-value-setq (value-ptr value-size)
             (funcall serializer value))
           (multiple-value-setq (new-ptr new-len)
             (remove-element (klist-pointer klist) (klist-size klist)
                             value-ptr value-size))
           (unless (pointer-eq new-ptr (klist-pointer klist))
             (kcfree (klist-pointer klist))
             (setf (klist-items klist)
                   (make-array 0 :adjustable t :fill-pointer t)
                   (klist-size klist) new-len
                   (klist-pointer klist) new-ptr
                   (klist-record klist) 0
                   (klist-offset klist) 0)
             ;; An empty list is no record; Kyoto's set ignores a NULL
             ;; value buffer (kcdb.h, BasicDB::set).
             (if (zerop new-len)
                 (dbm-remove-fast (klist-db klist)
                                  (klist-key klist) (klist-key-size klist))
                 (dbm-put-fast (klist-db klist)
                               (klist-key klist) (klist-key-size klist)
                               new-ptr new-len :mode :replace))))
      (progn
        (when (pointerp value-ptr) (kcfree value-ptr))
        (when (and (pointerp new-ptr)
                   (not (null-pointer-p new-ptr))
                   (not (pointer-eq new-ptr (klist-pointer klist))))
          (kcfree new-ptr)))))
  klist)

(defmethod klist-next ((klist klist))
  (when (< (klist-offset klist) (klist-size klist))
    (multiple-value-bind (data-length header-length)
        (extract-length (klist-pointer klist) (klist-offset klist))
      ;; Each element's own type byte, at its offset (was 0: every
      ;; element read as the first one's type).
      (multiple-value-bind (item record-size)
          (deserialize-subseq (klist-pointer klist)
                              header-length
                              data-length (klist-offset klist))
        (declare (ignore record-size))
        (let ((new-offset (+ data-length header-length)))
          (vector-push-extend item (klist-items klist))
          (incf (klist-record klist))
          (setf (klist-offset klist) new-offset)
          item)))))

(defmethod klist-prev ((klist klist))
  (when (> (klist-record klist) 0)
    (aref (klist-items klist) (1- (klist-record klist)))))

(defmethod klist-nth ((klist klist) (nth integer))
  "The NTH value, reading on as needed; NIL past the end.  A stored NIL
is a value, so reading stops at the end of the buffer, not at a NIL."
  (loop while (and (<= (klist-record klist) nth)
                   (< (klist-offset klist) (klist-size klist)))
        do (klist-next klist))
  (when (< nth (klist-record klist))
    (aref (klist-items klist) nth)))

(defmethod klist-has-value? ((klist klist) value)
  "Byte offset of VALUE's element in KLIST, or NIL."
  (let (value-ptr value-size)
    (unwind-protect
         (progn
           (multiple-value-setq (value-ptr value-size) (serialize value))
           (element-position (klist-pointer klist) (klist-size klist)
                             value-ptr value-size))
      (when (pointerp value-ptr) (kcfree value-ptr)))))

(defmethod map-klist ((fn function) (klist klist) &key collect?)
  "Call FN on every value in KLIST, in order; with COLLECT?, return the
results as a list."
  (loop while (< (klist-offset klist) (klist-size klist))
        do (klist-next klist))
  (loop for item across (klist-items klist)
        if collect? collect (funcall fn item)
        else do (funcall fn item)))
