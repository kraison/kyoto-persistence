(in-package #:kyoto-persistence)

;(declaim (optimize (speed 3)))

(defgeneric serialize (object))
(defgeneric serialize-special (object serialize-as))
(defgeneric compute-serialized-length (object))
(defgeneric deserialize (object size))
(defgeneric deserialize-help (become object header-length data-length))
(defgeneric deserialize-subseq (pointer header-length data-length offset))
(defgeneric make-serialized-key (key-type object))

(let ((length-table (make-hash-table :synchronized t)))
  (defun encode-length (int)
    (declare (type integer int))
    (or (gethash int length-table)
	(let* ((n-bytes (ceiling (integer-length int) 8))
	       (original-int int)
	       (vec (make-array (+ 1 n-bytes) :element-type '(unsigned-byte 8))))
	  (setf (aref vec 0) n-bytes)
	  (dotimes (i n-bytes)
	    (setf (aref vec (+ 1 i)) (ldb (byte 8 0) int))
	    (setq int (ash int -8)))
	  (setf (gethash original-int length-table) vec)))))

(defun decode-length (pointer start n-bytes)
  (let ((int 0))
    (dotimes (i n-bytes)
      (setq int (dpb (mem-aref pointer :unsigned-char (+ i start)) (byte 8 (* i 8)) int)))
    int))

(defun extract-length (pointer &optional (offset 0))
  (let ((id-byte (mem-aref pointer :unsigned-char (+ offset 0))))
    (cond ((or (= id-byte +uuid+) ;; These are all fixed length
	       (= id-byte +positive-integer+)
	       (= id-byte +negative-integer+)
	       (= id-byte +character+)
	       (= id-byte +single-float+)
	       (= id-byte +double-float+)
	       (= id-byte +timestamp+))
	   (values (mem-aref pointer :unsigned-char (+ offset 1)) (+ offset 2)))
	  ((or (= id-byte +t+) (= id-byte +null+))
	   (values 1 (+ offset 0)))
	  (t ;; strings, lists, vectors, blobs, nodes, triples have variable bytes
	   (let ((header-length (+ 2 offset (mem-aref pointer :unsigned-char (+ offset 1)))))
	     (values (decode-length pointer (+ offset 2) (- header-length 2 offset)) 
		     header-length))))))

(defmethod deserialize-subseq (pointer header-length data-length offset)
  (deserialize-help (mem-aref pointer :unsigned-char offset) 
		    pointer header-length data-length))

(defun deserialize-all-subseqs (pointer size &optional (offset 0))
  (cond ((= offset size)
	 nil)
	(t 
	 (multiple-value-bind (data-length header-length) (extract-length pointer offset)
	   (cons (deserialize-subseq pointer header-length data-length offset)
		 (deserialize-all-subseqs pointer size (+ header-length data-length)))))))

(defmethod deserialize :around (pointer size)
  (handler-case
      (call-next-method)
    (error (condition)
      (error 'deserialization-error :instance pointer :reason condition))))

(defmethod serialize :around (object)
  (handler-case
      (call-next-method)
    (error (condition)
      (error 'serialization-error :instance object :reason condition))))

(defmethod deserialize (pointer size)
  (multiple-value-bind (data-length header-length) (extract-length pointer)
    (deserialize-help (mem-aref pointer :unsigned-char 0) pointer header-length data-length)))

(defmethod make-serialized-key (key-type object)
  (multiple-value-bind (key-ptr key-size) (serialize object)
    (setf (mem-aref key-ptr :unsigned-char 0) key-type)
    (values key-ptr key-size)))

(defmethod deserialize-help ((become (eql +uuid+)) pointer header-length data-length)
  "Decode a UUID."
  (declare (type integer become))
  (declare (ignore header-length data-length))
  (values (uuid:pointer-to-uuid pointer 2) (+ header-length data-length)))

(defmethod serialize ((uuid uuid:uuid))
  "Encode a UUID."
  (uuid:uuid-to-pointer uuid +uuid+))

(defmethod deserialize-help ((become (eql +positive-integer+)) pointer header-length data-length)
  "Decode a positive integer."
  (declare (type integer become data-length header-length))
  (let ((int 0))
    (dotimes (i data-length)
      (setq int (dpb (mem-aref pointer :unsigned-char (+ header-length i)) (byte 8 (* i 8)) int)))
    (values int (+ header-length data-length))))
 
(defmethod deserialize-help ((become (eql +negative-integer+)) pointer header-length data-length)
  "Decode a negative integer."
  (declare (type integer data-length header-length))
  (declare (ignore become))
  (values (- (deserialize-help +positive-integer+ pointer header-length data-length))
	  (+ header-length data-length)))

(defmethod compute-serialized-length ((int integer))
  (let ((n-bytes (ceiling (integer-length int) 8)))
    (list (+ 2 n-bytes) n-bytes 2 1)))

(defmethod serialize ((int integer))
  "Encodes integers between (- (1- (expt 2 (* 8 255)))) and (1- (expt 2 (* 8 255)))"
  (destructuring-bind (size n-bytes header-length encoded-length) (compute-serialized-length int)
    (declare (ignore header-length encoded-length))
    (let ((vec (foreign-alloc :unsigned-char :count size)))
      (if (minusp int)
	  (progn
	    (setf (mem-aref vec :unsigned-char 0) +negative-integer+)
	    (setq int (abs int)))
	  (setf (mem-aref vec :unsigned-char 0) +positive-integer+))
      (setf (mem-aref vec :unsigned-char 1) n-bytes)
      (dotimes (i n-bytes)
	(setf (mem-aref vec :unsigned-char (+ 2 i)) (ldb (byte 8 0) int))
	(setq int (ash int -8)))
      (values vec size))))

(defmethod deserialize-help ((become (eql +single-float+)) pointer header-length data-length)
  (declare (type integer become header-length data-length))
  (ieee-floats:decode-float32 
   (values (deserialize-help +positive-integer+ pointer header-length data-length)
	   (+ header-length data-length))))

(defmethod serialize ((float single-float))
  (multiple-value-bind (ptr len) (serialize (ieee-floats:encode-float32 float))
    (setf (mem-aref ptr :unsigned-char 0) +single-float+)
    (values ptr len)))

(defmethod serialize ((float double-float))
  (multiple-value-bind (ptr len) (serialize (ieee-floats:encode-float64 float))
    (setf (mem-aref ptr :unsigned-char 0) +double-float+)
    (values ptr len)))

(defmethod deserialize-help ((become (eql +double-float+)) pointer header-length data-length)
  (declare (type integer become header-length data-length))
  (ieee-floats:decode-float64
   (values (deserialize-help +positive-integer+ pointer header-length data-length)
	   (+ header-length data-length))))

(defmethod serialize ((timestamp timestamp))
  (multiple-value-bind (ptr len) (serialize (timestamp-to-universal timestamp))
    (setf (mem-aref ptr :unsigned-char 0) +timestamp+)
    (values ptr len)))

(defmethod deserialize-help ((become (eql +timestamp+)) pointer header-length data-length)
  (declare (type integer become header-length data-length))
  (let ((universal-time (deserialize-help +positive-integer+ pointer header-length data-length)))
    (values (universal-to-timestamp universal-time)
	    (+ header-length data-length))))

(defmethod deserialize-help ((become (eql +ratio+)) pointer header-length data-length)
  (declare (type integer become header-length data-length))
  (let ((values (deserialize-all-subseqs pointer (+ header-length data-length) header-length)))
    (values (/ (first values) (second values)) (+ header-length data-length))))

(defmethod serialize ((ratio ratio))
  (let* ((numerator (numerator ratio))
	 (denominator (denominator ratio))
	 (numerator-bytes (ceiling (integer-length numerator) 8))
	 (denominator-bytes (ceiling (integer-length denominator) 8))
	 (data-length (+ 4 numerator-bytes denominator-bytes))
	 (encoded-length (encode-length data-length))
	 (length-of-encoded-length (length encoded-length))
	 (total-length (+ 1 length-of-encoded-length data-length)))
    (let ((vec (foreign-alloc :unsigned-char :count total-length)))
      (flet ((serialize-int (int n-bytes offset)
	       (if (minusp int)
		   (progn
		     (setf (mem-aref vec :unsigned-char offset) +negative-integer+)
		     (setq int (abs int)))
		   (setf (mem-aref vec :unsigned-char offset) +positive-integer+))
	       (setf (mem-aref vec :unsigned-char (1+ offset)) n-bytes)
	       (dotimes (i n-bytes)
		 (setf (mem-aref vec :unsigned-char (+ 2 i offset)) (ldb (byte 8 0) int))
		 (setq int (ash int -8)))
	       (values)))
	(setf (mem-aref vec :unsigned-char 0) +ratio+)
	(dotimes (i length-of-encoded-length)
	  (setf (mem-aref vec :unsigned-char (+ 1 i)) (aref encoded-length i)))
	(serialize-int numerator numerator-bytes (1+ length-of-encoded-length))
	(serialize-int denominator denominator-bytes 
		       (+ 1 length-of-encoded-length 2 numerator-bytes))
	(values vec total-length)))))

(defmethod deserialize-help ((become (eql +character+)) pointer header-length data-length)
  "Decode a Unicode-encoded byte sequence."
  (declare (type integer become header-length data-length))
  (let ((int 0))
    (dotimes (i data-length)
      (setq int (dpb (mem-aref pointer :unsigned-char (+ header-length i)) (byte 8 (* i 8)) int)))
    (values (code-char int) (+ header-length data-length))))

(defmethod serialize ((char character))
  "Encode a Unicode character."
  (let* ((code (char-code char))
	 (total-bytes (ceiling (integer-length code) 8))
	 (vec (foreign-alloc :unsigned-char :count (+ 2 total-bytes))))
    (setf (mem-aref vec :unsigned-char 0) +character+)
    (setf (mem-aref vec :unsigned-char 1) total-bytes)
    (dotimes (i total-bytes)
      (setf (mem-aref vec :unsigned-char (+ 2 i)) (ldb (byte 8 0) code))
      (setq code (ash code -8)))
    (values vec (+ 2 total-bytes))))

(defmethod deserialize-help ((become (eql +string+)) pointer header-length data-length)
  (declare (type integer become header-length data-length))
  (let ((bytes (make-array data-length :fill-pointer t :adjustable t 
			   :element-type '(unsigned-byte 8))))
    (dotimes (i data-length)
      (setf (aref bytes i) (mem-aref pointer :unsigned-char (+ i header-length))))
    (values (sb-ext:octets-to-string bytes) (+ header-length data-length))))

(defmethod serialize ((string string))
  "Unicode aware string encoding. Not as efficient as it could be: creates 2 arrays: one to get 
sbcl's internal byte representation of the string, and then another for prepending our code and
the length of the object."
  (let* ((unicode (sb-ext:string-to-octets string))
	 (vector-length (length unicode))
	 (encoded-length (encode-length vector-length))
	 (length-of-encoded-length (length encoded-length))
	 (vec (foreign-alloc :unsigned-char :count (+ 1 length-of-encoded-length vector-length))))
    (setf (mem-aref vec :unsigned-char 0) +string+)
    (dotimes (i length-of-encoded-length)
      (setf (mem-aref vec :unsigned-char (1+ i)) (aref encoded-length i)))
    (dotimes (i vector-length)
      (setf (mem-aref vec :unsigned-char (+ 1 length-of-encoded-length i)) (aref unicode i)))
    (values vec (+ 1 length-of-encoded-length vector-length))))

(defmethod deserialize-help ((become (eql +t+)) pointer header-length data-length)
  (declare (type integer become header-length data-length))
  (declare (ignore pointer))
  (values t (+ header-length data-length)))

(defmethod deserialize-help ((become (eql +null+)) pointer header-length data-length)
  (declare (type integer become header-length data-length))
  (declare (ignore pointer))
  (values nil (+ header-length data-length)))

(defmethod deserialize-help ((become (eql +symbol+)) pointer header-length data-length)
  (declare (type integer become header-length data-length))
  (let ((values (deserialize-all-subseqs pointer (+ header-length data-length) header-length)))
    (values (intern (first values) (find-package (second values)))
	    (+ header-length data-length))))

(defmethod serialize ((symbol symbol))
  (or (and (null symbol) 
	   (let ((vec (foreign-alloc :unsigned-char :count 1)))
	     (setf (mem-aref vec :unsigned-char 0) +null+)
	     vec))
      (and (eq symbol t)
	   (let ((vec (foreign-alloc :unsigned-char :count 1)))
	     (setf (mem-aref vec :unsigned-char 0) +t+)
	     vec))
      (let* ((symbol-name (sb-ext:string-to-octets (symbol-name symbol)))
	     (symbol-length (length symbol-name))
	     (symbol-encoded-length (encode-length symbol-length)) 
	     (package-name (sb-ext:string-to-octets (package-name (symbol-package symbol))))
	     (package-length (length package-name))
	     (package-encoded-length (encode-length package-length))
	     (size (+ symbol-length package-length 
		      (length symbol-encoded-length) (length package-encoded-length)))
	     (encoded-length (encode-length (+ 2 size)))
	     (vec (foreign-alloc :unsigned-char :count (+ 1 (length encoded-length) size))))
	(setf (mem-aref vec :unsigned-char 0) +symbol+)
	(dotimes (i (length encoded-length))
	  (setf (mem-aref vec :unsigned-char (1+ i)) (aref encoded-length i)))

	(setf (mem-aref vec :unsigned-char (+ 1 (length encoded-length))) +string+)
	(dotimes (i (length symbol-encoded-length))
	  (setf (mem-aref vec :unsigned-char (+ 2 (length encoded-length) i)) 
		(aref symbol-encoded-length i)))
	(dotimes (i (length symbol-name))
	  (setf (mem-aref vec :unsigned-char 
			  (+ 2 (length encoded-length) (length symbol-encoded-length) i))
		(aref symbol-name i)))

	(setf (mem-aref vec :unsigned-char (+ 2 (length encoded-length) 
					      (length symbol-encoded-length) (length symbol-name)))
	      +string+)
	(dotimes (i (length package-encoded-length))
	  (setf (mem-aref vec :unsigned-char 
			  (+ 3 i (length encoded-length) (length symbol-encoded-length) 
			     (length symbol-name)))
		(aref package-encoded-length i)))
	(dotimes (i (length package-name))
	  (setf (mem-aref vec :unsigned-char 
			  (+ 3 i (length encoded-length) (length symbol-encoded-length) 
			     (length symbol-name) (length package-encoded-length)))
		(aref package-name i)))
	(values vec (+ 1 (length encoded-length) size)))))


#|
(defmethod deserialize-help ((become (eql +list+)) pointer header-length data-length)
  (declare (type integer become header-length data-length))
  (deserialize-all-subseqs pointer (+ header-length data-length) header-length))

(defmethod deserialize-help ((become (eql +dotted-list+)) pointer header-length data-length)
  (declare (type integer become header-length data-length))
  (let* ((items (deserialize-all-subseqs pointer (+ header-length data-length) header-length))
	 (result nil))
    (loop for i downfrom (- (length items) 2) to 0 do
      (push (nth i items) result))
    (nconc result (car (last items)))))

(defmethod serialize ((list list))
  (if (proper-listp list)
      (let* ((serialized-items (mapcar #'serialize list))
	     (total-length (reduce #'+ (mapcar #'length serialized-items)))
	     (encoded-length (encode-length total-length))
	     (length-of-encoded-length (length encoded-length))
	     (vec (make-array 1 :fill-pointer 0 :adjustable t :element-type '(unsigned-byte 8))))
	(vector-push-extend +list+ vec)
	(dotimes (i length-of-encoded-length)
	  (vector-push-extend (aref encoded-length i) vec))
	(dolist (item serialized-items)
	  (dotimes (i (length item))
	    (vector-push-extend (aref item i) vec)))
	vec)
      (let ((vec (make-array 1 :fill-pointer 0 :adjustable t :element-type '(unsigned-byte 8)))
	    (serialized-items nil))
	(loop for elt on list do
	     (push (serialize (car elt)) serialized-items)
	     (when (atom (cdr elt))
	       ;; The last element
	       (push (serialize (cdr elt)) serialized-items)))
	(let* ((total-length (reduce #'+ (mapcar #'length serialized-items)))
	       (encoded-length (encode-length total-length))
	       (length-of-encoded-length (length encoded-length)))
	  (vector-push-extend +dotted-list+ vec)
	  (dotimes (i length-of-encoded-length)
	    (vector-push-extend (aref encoded-length i) vec))
	  (dolist (item (reverse serialized-items))
	    (dotimes (i (length item))
	      (vector-push-extend (aref item i) vec)))
	  vec))))
|#
