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
  :components ((:file "uuid")
	       (:file "kyoto-persistence-package" :depends-on ("uuid"))
	       (:file "constants" :depends-on ("kyoto-persistence-package"))
	       (:file "utilities" :depends-on ("constants"))
	       (:file "serialize" :depends-on ("utilities"))
	       (:file "kyoto-list" :depends-on ("serialize"))
	       (:file "disk-storage" :depends-on ("kyoto-list"))
	       (:file "phash" :depends-on ("disk-storage"))
	       (:file "btree" :depends-on ("disk-storage"))))


