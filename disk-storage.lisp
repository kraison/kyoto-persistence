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
	     (with-transaction (db)
	       (multiple-value-setq (key-ptr key-size) (funcall serializer key))
	       (multiple-value-setq (value-ptr value-size) (dbm-get-fast db key-ptr key-size))
	       (when (and value-ptr (not (null-pointer-p value-ptr)))
		 (deserialize value-ptr value-size)))
	  (progn
	    (when (pointerp key-ptr) (kcfree key-ptr))
	    (when (pointerp value-ptr) (kcfree value-ptr)))))
    (error (condition)
      (error 'persistence-error :instance (list :db db :key key) :reason condition))))    

(defun lookup-objects (db key &key (serializer #'serialize))
  "Lookup up multiple objects with the same key. Returns a klist structure.  Caller must free the
pointers in this structure with klist-free!"
  (let (key-ptr key-size value-ptr value-size)
    (handler-case
	(with-transaction (db)
	  (multiple-value-setq (key-ptr key-size) (funcall serializer key))
	  (multiple-value-setq (value-ptr value-size) (dbm-get-fast db key-ptr key-size))
	  (if (null-pointer-p value-ptr)
	      nil
	      (make-klist 
	       :db db :key key-ptr :key-size key-size :pointer value-ptr :size value-size)))
    (error (condition)
      (when (pointerp key-ptr) (kcfree key-ptr))
      (when (pointerp value-ptr) (kcfree value-ptr))
      (error 'persistence-error :instance (list :db db :key key) :reason condition)))))

(defun store-object (db key value &key (mode :keep) (key-serializer #'serialize) 
		     (value-serializer #'serialize))
  "Save a key / value pair in the specified kyoto cabinet store."
  (format t "Storing ~A / ~A~%" key value)
  (handler-case
      (let (key-ptr key-size value-ptr value-size)
	(unwind-protect
	     (progn
	       (multiple-value-setq (key-ptr key-size) (funcall key-serializer key))
	       (multiple-value-setq (value-ptr value-size) (funcall value-serializer value))
	       (dump-pointer key-ptr key-size)
	       (dump-pointer value-ptr value-size)
	       (with-transaction (db)
		 (dbm-put-fast db key-ptr key-size value-ptr value-size :mode mode))
	       (format t "Done storing ~A / ~A~%" key value))
	  (progn
	    (format t "Releasing key ptr: ~A~%" (pointer-address key-ptr))
	    (when (pointerp key-ptr) (kcfree key-ptr))
	    (when (pointerp value-ptr)
	      (format t "Releasing val ptr: ~A~%" (pointer-address value-ptr))
	      (kcfree value-ptr)))))
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
		   (with-transaction (db)
		     (dbm-remove-fast key-ptr key-size)))
	      (progn
		(when (pointerp key-ptr) (kcfree key-ptr)))))
	  (with-transaction (db)
	    (let ((klist (lookup-objects db key :serializer key-serializer)))
	      (when (klist? klist)
		(klist-remove klist value)))))
    (error (condition)
      (error 'persistence-error 
	     :instance (list :db db :key key :value value) :reason condition))))

