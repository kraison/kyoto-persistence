(in-package #:kyoto-persistence)

(defun open-store (file)
  "Open a kyoto cabinet database.  The type depends on the file extension. If its suffix is .kch, 
the database will be a file hash database. If its suffix is .kct, the database will be a file tree 
database. If its suffix is .kcd, the database will be a directory hash database. If its suffix is 
.kcf, the database will be a directory tree database."
  (handler-case
      (let ((db (make-instance 'kc-dbm)))
	(dbm-open db file :READ :WRITE :CREATE)
	db)
    (error (condition)
      (error 'persistence-error :instance file :reason condition))))

(defun close-store (db)
  "Close a kyoto cabinet data store."
  (handler-case
      (dbm-close db)
    (error (condition)
      (error 'persistence-error :instance db :reason condition))))

(defun lookup-object (db key &key (serializer #'serialize))
  "Lookup an object by its key."
  (handler-case
      (let (key-ptr key-size value-ptr value-size)
	(unwind-protect
	     (progn
	       (multiple-value-setq (key-ptr key-size) (funcall serializer key))
	       (multiple-value-setq (value-ptr value-size) (dbm-get-fast db key-ptr key-size))
	       (deserialize value-ptr value-size))
	  (progn
	    (when (pointerp key-ptr) (foreign-free key-ptr))
	    (when (pointerp value-ptr) (foreign-free value-ptr)))))
    (error (condition)
      (error 'persistence-error :instance (list :db db :key key) :reason condition))))    

(defun lookup-objects (db key &key (serializer #'serialize))
  "Lookup up multiple objects with the same key."
  (lookup-object db key :serializer serializer))

(defun store-object (db key value &key (mode :keep) (key-serializer #'serialize) 
		     (value-serializer #'serialize))
  "Save a key / value pair in the specified kyoto cabinet store."
  (handler-case
      (let (key-ptr key-size value-ptr value-size)
	(unwind-protect
	     (progn
	       (multiple-value-setq (key-ptr key-size) (funcall key-serializer key))
	       (multiple-value-setq (value-ptr value-size) (funcall value-serializer value))
	       (dbm-put-fast db key-ptr key-size value-ptr value-size :mode mode)
	       t)
	  (progn
	    (when (pointerp key-ptr) (foreign-free key-ptr))
	    (when (pointerp value-ptr) (foreign-free value-ptr)))))
    (error (condition)
      (error 'persistence-error 
	     :instance (list :db db :key key :value value :mode mode) 
	     :reason condition))))

(defun delete-object (db key &key value (key-serializer #'serialize) (value-serializer #'serialize))
  "Remove an object from the kyoto cabinet store."
  (handler-case
      (if (null value)
	  (let (key-ptr key-size)
	    (unwind-protect
		 (progn
		   (multiple-value-setq (key-ptr key-size) (funcall key-serializer key))
		   (dbm-remove-fast key-ptr key-size))
	      (progn
		(when (pointerp key-ptr) (foreign-free key-ptr)))))
	  (error "Cannot delete specific kv pairs yet."))
    (error (condition)
      (error 'persistence-error 
	     :instance (list :db db :key key :value value) :reason condition))))

#|	  
(defun extract-subseq (seq seq1)
  "Return a copy of a sequence with a subsequence removed."
  (declare (optimize (speed 3)))
  (let* ((rem-len (length seq1))
	 (old-len (length seq)))
    (when (> rem-len old-len)
      (error "Attempt to remove a subsequence that is longer than the original! ~A / ~A" seq seq1))
    (let* ((position (search seq1 seq))
	   (new-len (- old-len rem-len))
	   (new-seq (make-array new-len)))
      (loop for i from 0 to (1- position) do
	   (setf (aref new-seq i) (aref seq i))) 
      (loop 
	 for i from (+ position rem-len) to (1- old-len)
	 for j from position to (1- new-len)
	 do
	 (setf (aref new-seq j) (aref seq i)))
      (format t "New array is ~A~%" new-seq)
      new-seq)))
  
(with-transaction (db)
	    (let ((val (dbm-get-fast db key :octets)))
	      (if (equalp value val)
		  (dbm-remove db key)
		  (let* ((rem-len (length value))
			 (position (search value val)))
		    (cond ((null position)
			   nil)
			  ((= position 0)
			   (dbm-put db key (subseq val rem-len) :mode :replace))
			  (t
			   (dbm-put db key (extract-subseq val value) :mode :replace))))))))
    (error (condition)
      (error 'persistence-error 
	     :instance (list :db db :key key :value value) :reason condition))))


	  (let ((cursor (iter-open db)))
	    (iter-go-to cursor key)
	    (loop
	       (multiple-value-bind (ikey ival) 
		   (iter-item cursor :key-type :octets :value-type :octets) 
		 (when (or (null ikey)
			   (= 0 (length ikey))
			   (not (equalp key ikey)))
		   (return))
		 (when (equal ival value)
		   (iter-remove cursor)
		   (return))
		 (iter-next cursor)))
	    t))
    (error (condition)
      (error 'persistence-error 
	     :instance (list :db db :key key :value value) :reason condition))))
|#

