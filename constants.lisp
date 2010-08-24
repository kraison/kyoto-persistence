(in-package #:kyoto-persistence)

(cffi:defctype size :unsigned-int)

;; Built-in type identifiers for serializing
(defconstant +unknown+ 0)
(defconstant +negative-integer+ 1)
(defconstant +positive-integer+ 2)
(defconstant +character+ 3)
(defconstant +symbol+ 4)
(defconstant +string+ 5)
(defconstant +single-float+ 6)
(defconstant +double-float+ 7)
(defconstant +ratio+ 8)
(defconstant +t+ 9)
(defconstant +null+ 10)
(defconstant +timestamp+ 11)
(defconstant +uuid+ 12)
(defconstant +slot-key+ 13)

(defconstant +blob+ 14) ;; Uninterpreted octets
(defconstant +dotted-list+ 15)
(defconstant +list+ 16)
(defconstant +vector+ 17)

