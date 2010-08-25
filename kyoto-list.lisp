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

(defmethod klist-free ((klist klist))
  (when (and (pointerp (klist-pointer klist)) (not (null-pointer-p (klist-pointer klist))))
    (foreign-free (klist-pointer klist)))
  (when (and (pointerp (klist-pointer klist)) (not (null-pointer-p (klist-key klist))))
    (foreign-free (klist-key klist))))

(defmethod klist-remove ((klist klist) value)
  (let (value-ptr value-size new-ptr new-len)
    (unwind-protect
	 (progn
	   (multiple-value-setq (value-ptr value-size) (serialize value))
	   (dump-pointer value-ptr value-size)
	   (multiple-value-setq (new-ptr new-len) 
	     (extract-subseq (klist-pointer klist) (klist-size klist) value-ptr value-size))
	   (format t "extract-subseq returned ~A / ~A~%" new-ptr new-len)
	   (when (and (pointerp new-ptr) new-len)
	     (when (not (pointer-eq new-ptr (klist-pointer klist)))
	       (foreign-free (klist-pointer klist))
	       (setf (klist-items klist) (make-array 0 :adjustable t :fill-pointer t)
		     (klist-size klist) new-len
		     (klist-pointer klist) new-ptr
		     (klist-record klist) 0
		     (klist-offset klist) 0)
	       (dbm-put-fast (klist-db klist) 
			     (klist-key klist) (klist-key-size klist)
			     value-ptr value-size :mode :replace))))
      (progn
	(when (pointerp value-ptr) (foreign-free value-ptr))
	(when (and (pointerp new-ptr) (not (pointer-eq new-ptr (klist-pointer klist))))
	  (foreign-free new-ptr)))))
  klist)

(defmethod klist-next ((klist klist))
  (when (< (klist-offset klist) (klist-size klist))
    (multiple-value-bind (data-length header-length) 
	(extract-length (klist-pointer klist) (klist-offset klist))
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
  (if (< nth (klist-record klist))
      (aref (klist-items klist) nth)
      (progn
	(loop for i from (klist-record klist) to nth do
	     (when (null (klist-next klist))
	       (return-from klist-nth nil)))
	(aref (klist-items klist) nth))))

(defmethod klist-has-value? ((klist klist) value)
  (let (value-ptr value-size)
    (unwind-protect
	 (progn
	   (multiple-value-setq (value-ptr value-size) (serialize value))
	   (pointer-search value-ptr value-size (klist-pointer klist) (klist-size klist)))
      (when (pointerp value-ptr) (foreign-free value-ptr)))))

(defmethod map-klist ((fn function) (klist klist) &key collect?)
  (let* ((count 0))
    (if collect?
	(let ((result nil))
	  (loop 
	     for item = (klist-nth klist count)
	     while (not (null item)) do
	       (push (funcall fn item) result)
	       (incf count))
	  (nreverse result))
	(progn
	  (loop 
	     for item = (klist-nth klist count)
	     while (not (null item)) do
	       (funcall fn item)
	       (incf count))
	  nil))))
