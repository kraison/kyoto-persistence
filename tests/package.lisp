(defpackage #:kyoto-persistence-test
  (:use #:cl #:fiveam)
  (:local-nicknames (#:kp #:kyoto-persistence)))

(in-package #:kyoto-persistence-test)

(def-suite :kyoto-persistence :description "kyoto-persistence tests")

(defun round-trip (object)
  "Serialize OBJECT, deserialize the bytes, free them; return the copy."
  (multiple-value-bind (ptr size) (kp:serialize object)
    (unwind-protect (kp:deserialize ptr size)
      (kyoto-cabinet-ffi:kcfree ptr))))

(defun temp-store-path (type)
  "A fresh path under /var/tmp for a store of TYPE, \"kch\" or \"kct\"."
  (format nil "/var/tmp/kp-test-~36r.~a"
          (random (expt 36 10) (make-random-state t)) type))
