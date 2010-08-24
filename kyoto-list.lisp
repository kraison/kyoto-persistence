(in-package #:kyoto-persistence)

(defstruct (klist
	     (:predicate klist?))
  pointer 
  (size 0 :type integer)
  (record 0 :type integer)
  (offset 0 :type integer)
  (items (make-array 0 :adjustable t :fill-pointer t)))

(defmethod klist-free ((klist klist))
  (foreign-free (klist-pointer klist)))

(defmethod klist-next ((klist klist))
  (when (< (klist-offset klist) (klist-size klist))
    (multiple-value-bind (data-length header-length) (extract-length pointer (klist-offset klist))
      (multiple-value-bind (item record-size) 
	  (deserialize-subseq (klist-pointer klist) 
			      (+ (klist-offset klist) header-length) 
			      data-length (klist-offset klist))
	(let ((new-offset (+ record-size (klist-offset klist))))
	  (incf (klist-record klist))
	  (setf (klist-offset klist) new-offset)
	  (vector-push-extend item (klist-items klist))
	  item)))))

(defmethod klist-prev ((klist klist))
  (when (> (klist-record klist) 0)
    (aref (1- (klist-record klist)) (klist-items klist))))

(defmethod klist-nth ((klist klist) nth)
  (if (<= nth (klist-record klist))
      (aref (klist-items klist) nth)
      (progn
	(loop until (= nth (klist-record klist)) do
	     (when (null (klist-next klist))
	       (error "~A is not a valid offset for ak list of size ~A" 
		      nth (1+ (klist-record klist)))))
	(aref (klist-items klist) nth))))
