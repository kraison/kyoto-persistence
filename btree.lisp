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
