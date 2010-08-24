(in-package #:kyoto-persistence)

(define-condition serialization-error (error)
  ((instance :initarg :instance)
   (reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (instance reason) error
               (format stream "Serialization failed for ~a because of ~a." instance reason)))))

(define-condition deserialization-error (error)
  ((instance :initarg :instance)
   (reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (instance reason) error
               (format stream "Deserialization failed for ~a because of ~a." instance reason)))))

(define-condition persistence-error (error)
  ((instance :initarg :instance)
   (reason :initarg :reason))
  (:report (lambda (error stream)
	     (with-slots (instance reason) error
	       (format stream "Persistence failed for ~a because of ~a." instance reason)))))

(define-condition transaction-error (error)
  ((instance :initarg :instance)
   (reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (instance reason) error
	       (format stream "Transaction failed for ~a because of ~a." instance reason)))))

