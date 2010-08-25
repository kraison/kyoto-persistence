(in-package #:cl-user)

(defpackage #:kyoto-persistence
  (:use #:cl #:cffi #:kyoto-cabinet #:kyoto-cabinet-ffi #:local-time)
  (:export 
   #:def-persistent-class

   #:open-store
   #:close-store
   #:store-object
   #:lookup-object
   #:lookup-objects
   #:delete-object
   #:with-transaction

   #:serialize
   #:serialize-special
   #:deserialize
   #:deserialize-help
   #:make-serialized-key

   #:open-phash
   #:close-phash
   #:set-phash
   #:get-phash
   #:rem-phash
   #:map-phash

   #:open-btree
   #:close-btree
   #:set-btree
   #:get-btree
   #:rem-btree
   #:map-btree

   #:klist-free
   #:klist-remove
   #:klist-next
   #:klist-prev
   #:klist-nth
   #:klist-has-value?
   #:map-klist

   #:serialization-error
   #:deserialization-error
   #:persistence-error
   #:transaction-error
   ))
