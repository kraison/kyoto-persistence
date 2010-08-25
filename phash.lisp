(in-package #:kyoto-persistence)

(defun open-phash (file)
  (open-store file))

(defun close-phash (phash)
  (close-store phash))

(defun set-phash (phash key value &key (mode :keep))
  (format t "phash storing ~A / ~A~%" key value)
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
	(iter-first cursor)
	(loop
	   (let (key-ptr key-size val-ptr val-size)
	     (unwind-protect
		  (progn
		    (multiple-value-setq (key-ptr key-size val-ptr val-size) 
		      (iter-item-fast cursor))
		    (when (null-pointer-p key-ptr) (return))
		    (let ((key (deserialize key-ptr (mem-ref key-size :int)))
			  (val (deserialize val-ptr (mem-ref val-size :int))))
		      (if collect?
			  (push (funcall func key val) result)
			  (funcall func key val)))
		    (iter-next cursor))
	       (progn
		 (when (pointerp key-ptr) (foreign-free key-ptr))
		 (when (pointerp key-size) (foreign-free key-size))
		 ;; This is commented out because it causes a segfault.
		 ;;(when (pointerp val-ptr) (foreign-free val-ptr))
		 (when (pointerp val-size) (foreign-free val-size))))))
	(if collect? (nreverse result)))
    (error (condition)
      (error 'persistence-error 
	     :instance (list :db phash) :reason condition))))
