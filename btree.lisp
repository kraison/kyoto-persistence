(in-package #:kyoto-persistence)

(defstruct (btree
	     (:predicate btree?))
  file db duplicates-allowed?)

(defun open-btree (file &key duplicates-allowed?)
  (make-btree :file file :db (open-store file) :duplicates-allowed? duplicates-allowed?))

(defun close-btree (btree)
  (close-store (btree-db btree)))

(defun set-btree (btree key value)
  (store-object (btree-db btree) 
		key value 
		:mode (if (btree-duplicates-allowed? btree)
			  :concat
			  :keep)))

(defun get-btree (btree key)
  (if (btree-duplicates-allowed? btree)
      (lookup-objects (btree-db btree) key)
      (lookup-object (btree-db btree) key)))

(defun rem-btree (btree key &optional value)
  (delete-object (btree-db btree) key :value value))

(defun map-btree (btree fn &key collect?)
  )