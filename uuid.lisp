(in-package #:uuid)

(asdf:oos 'asdf:load-op 'cffi)
(asdf:oos 'asdf:load-op 'cl-kyoto-cabinet)

(export 'time-low)
(export 'time-mid)
(export 'time-high)
(export 'clock-seq-var)
(export 'clock-seq-low)
(export 'node)
(export 'time-high-and-version)
(export 'clock-seq-and-reserved)
(export 'uuid-eql)
(export 'uuid?)
(export 'pointer-to-uuid)
(export 'uuid-to-pointer)

(defgeneric uuid? (thing)
  (:method ((thing uuid)) t)
  (:method (thing) nil)
  (:documentation "UUID type predicate."))

(defgeneric uuid-eql (uuid1 uuid2)
  (:method ((uuid1 uuid) (uuid2 uuid))
    (equalp (uuid-to-byte-array uuid1) (uuid-to-byte-array uuid2)))
  (:method ((uuid1 uuid) uuid2)
    nil)
  (:method (uuid1 (uuid2 uuid))
    nil)
  (:documentation "Equality check for UUIDs."))

(defun uuid-to-pointer (uuid &optional (type-specifier nil))
  "Converts a uuid to a cffi pointer.  Caller must free the pointer!"
  (if type-specifier
      ;;(let ((array (cffi:foreign-alloc :unsigned-char :count 18)))
      (let* ((sz (cffi:foreign-alloc :int :initial-element 18))
	     (array (kyoto-cabinet-ffi:kcmalloc sz)))
	(setf (cffi:mem-aref array :unsigned-char 0) type-specifier)
	(setf (cffi:mem-aref array :unsigned-char 1) 16)
	(with-slots 
	      (time-low time-mid time-high-and-version clock-seq-and-reserved clock-seq-low node)
	    uuid
	  (loop for i from 3 downto 0
	     do (setf (cffi:mem-aref array :unsigned-char (+ 2 (- 3 i))) 
		      (ldb (byte 8 (* 8 i)) time-low)))
	  (loop for i from 5 downto 4
	     do (setf (cffi:mem-aref array :unsigned-char (+ 2 i)) 
		      (ldb (byte 8 (* 8 (- 5 i))) time-mid)))
	  (loop for i from 7 downto 6
	     do (setf (cffi:mem-aref array :unsigned-char (+ 2 i)) 
		      (ldb (byte 8 (* 8 (- 7 i))) time-high-and-version)))
	  (setf (cffi:mem-aref array :unsigned-char (+ 2 8)) 
		(ldb (byte 8 0) clock-seq-and-reserved))
	  (setf (cffi:mem-aref array :unsigned-char (+ 2 9)) 
		(ldb (byte 8 0) clock-seq-low))
	  (loop for i from 15 downto 10
	     do (setf (cffi:mem-aref array :unsigned-char (+ 2 i)) 
		      (ldb (byte 8 (* 8 (- 15 i))) node)))
	  (cffi:foreign-free sz)
	  (values array 18)))
      ;;(let ((array (cffi:foreign-alloc :unsigned-char :count 16)))
      (let* ((sz (cffi:foreign-alloc :int :initial-element 16))
	     (array (kyoto-cabinet-ffi:kcmalloc sz)))
	(with-slots 
	      (time-low time-mid time-high-and-version clock-seq-and-reserved clock-seq-low node)
	    uuid
	  (loop for i from 3 downto 0
	     do (setf (cffi:mem-aref array :unsigned-char (- 3 i)) (ldb (byte 8 (* 8 i)) time-low)))
	  (loop for i from 5 downto 4
	     do (setf (cffi:mem-aref array :unsigned-char i) (ldb (byte 8 (* 8 (- 5 i))) time-mid)))
	  (loop for i from 7 downto 6
	     do (setf (cffi:mem-aref array :unsigned-char i) 
		      (ldb (byte 8 (* 8 (- 7 i))) time-high-and-version)))
	  (setf (cffi:mem-aref array :unsigned-char 8) (ldb (byte 8 0) clock-seq-and-reserved))
	  (setf (cffi:mem-aref array :unsigned-char 9) (ldb (byte 8 0) clock-seq-low))
	  (loop for i from 15 downto 10
	     do (setf (cffi:mem-aref array :unsigned-char i) (ldb (byte 8 (* 8 (- 15 i))) node)))
	  (cffi:foreign-free sz)
	  (values array 16)))))

(defmacro marr-to-bytes (from to array)
  "Helper macro used in pointer-to-uuid."
  `(loop for i from ,from to ,to
         with res = 0
         do (setf (ldb (byte 8 (* 8 (- ,to i))) res) (cffi:mem-aref ,array :unsigned-char i))
         finally (return res)))

(defun pointer-to-uuid (pointer offset)
  "Converts a pointer generated with uuid-to-pointer to an uuid."
  (declare (type integer offset))
  (make-instance 'uuid
                 :time-low (marr-to-bytes (+ offset 0) (+ offset 3) pointer)
                 :time-mid (marr-to-bytes (+ offset 4) (+ offset 5) pointer)
                 :time-high (marr-to-bytes (+ offset 6) (+ offset 7) pointer)
                 :clock-seq-var (cffi:mem-aref pointer :unsigned-char (+ offset 8))
                 :clock-seq-low (cffi:mem-aref pointer :unsigned-char (+ offset 9))
                 :node (marr-to-bytes (+ offset 10) (+ offset 15) pointer)))
