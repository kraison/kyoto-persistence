(in-package #:kyoto-persistence)

(defun open-phash (file)
  (open-store file))

(defun close-phash (phash)
  (close-store phash))

(defun set-phash (phash key value &key (mode :keep))
  (store-object phash key value :mode mode))

(defun get-phash (phash key)
  (lookup-object phash key))

(defun rem-phash (phash key)
  (delete-object phash key))

(defun map-phash (func phash &key collect?)
  "Apply function FUNC to all entries in a kyoto hash database. FUNC should take two arguments:
key and value."
  (handler-case
      (let ((result nil) (cursor (iter-open phash)))
	(unwind-protect
	     (progn
	       (iter-first cursor)
	       (loop
		  (let (key-ptr key-size val-ptr val-size)
		    (unwind-protect
			 (progn
			   (multiple-value-setq (key-ptr key-size val-ptr val-size) 
			     (iter-item-fast cursor))
			   (when (or (null key-ptr)
				     (and (pointerp key-ptr) (null-pointer-p key-ptr))) 
			     (return))
			   (let ((key (deserialize key-ptr (mem-ref key-size :unsigned-int)))
				 (val (deserialize (mem-ref val-ptr :pointer) 
						   (mem-ref val-size :unsigned-int))))
			     (if collect?
				 (push (funcall func key val) result)
				 (funcall func key val)))
			   (iter-next cursor))
		      (progn
			(when (pointerp key-ptr) (kcfree key-ptr))
			(when (pointerp key-size) (foreign-free key-size))
			(when (pointerp val-ptr) (kcfree val-ptr))
			(when (pointerp val-size) (foreign-free val-size))))))
	       (if collect? (nreverse result)))
	  (iter-close cursor)))
    (error (condition)
      (error 'persistence-error 
	     :instance (list :db phash) :reason condition))))
