;; ASDF package description for vivace-graph              -*- Lisp -*-

(defpackage :kyoto-persistence-system (:use :cl :asdf))
(in-package :kyoto-persistence-system)

(defsystem kyoto-persistence
  :name "Kyoto Cabinet Data Persistence"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "0.1"
  :description "Kyoto Cabinet Data Persistence"
  :long-description "Kyoto Cabinet Data Persistence."
  :depends-on (:cl-kyoto-cabinet
               :cffi
               :ieee-floats
               :uuid
               :local-time)
  :in-order-to ((test-op (test-op :kyoto-persistence/test)))
  :components ((:file "uuid")
               (:file "kyoto-persistence-package" :depends-on ("uuid"))
               (:file "constants" :depends-on ("kyoto-persistence-package"))
               (:file "conditions" :depends-on ("constants"))
               (:file "utilities" :depends-on ("conditions"))
               (:file "serialize" :depends-on ("utilities"))
               (:file "kyoto-list" :depends-on ("serialize"))
               (:file "disk-storage" :depends-on ("kyoto-list"))
               (:file "phash" :depends-on ("disk-storage"))
               (:file "btree" :depends-on ("disk-storage"))
               ;;(:file "mop" :depends-on ("btree" "phash"))
               ))

(defsystem kyoto-persistence/test
  :description "FiveAM tests for kyoto-persistence."
  :depends-on (:kyoto-persistence :fiveam :cl-ppcre)
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "serialize-tests")
               (:file "buffer-tests"))
  :perform (test-op (o c)
             (unless (uiop:symbol-call :fiveam :run! :kyoto-persistence)
               (error "kyoto-persistence tests failed"))))


