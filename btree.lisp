(in-package #:kyoto-persistence)

(defstruct (btree
	     (:predicate btree?))
  file db duplicates-allowed?)

(defun open-btree (file &key duplicates-allowed?)
  (make-btree :file file :db (open-store file) :duplicates-allowed? duplicates-allowed?))

(defun close-btree (btree)
  (close-store (btree-db btree)))

(defun set-btree (btree key value &key (mode :keep) (key-serializer #'serialize)
		  (value-serializer #'serialize))
  (store-object (btree-db btree) 
		key value 
		:mode mode
		:key-serializer key-serializer
		:value-serializer value-serializer))

(defun get-btree (btree key &key (mode :single) (serializer #'serialize))
  (if (eql mode :klist)
      (lookup-objects (btree-db btree) key :serializer serializer)
      (lookup-object (btree-db btree) key :serializer serializer)))

(defun rem-btree (btree key &key value (key-serializer #'serialize)
		  (value-serializer #'serialize))
  (delete-object (btree-db btree) 
		 key :value value 
		 :key-serializer key-serializer :value-serializer value-serializer))

(defun map-btree (btree fn &key collect?)
  )

(defmacro with-transaction ((db) &body body)
  "Evaluates BODY in the context of a transaction on DB. If no
transaction is in progress, a new one is started. If a transaction is
already in progress, BODY is evaluated in its context. If an error
occurs, the transaction will rollback, otherwise it will commit."
  (let ((success (gensym)))
    `(let ((,success nil))
       (flet ((atomic-op ()
                ,@body))
         (if *in-transaction-p*
             (atomic-op)
             (unwind-protect
                  (let ((*in-transaction-p* t))
                    (prog2
                        (dbm-begin (if (btree? ,db) (btree-db ,db) ,db))
                        (atomic-op)
                      (setf ,success t)))
               (if ,success
                   (dbm-commit (if (btree? ,db) (btree-db ,db) ,db))
                   (dbm-rollback (if (btree? ,db) (btree-db ,db) ,db)))))))))

