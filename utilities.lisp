(in-package #:kyoto-persistence)

(defun pointer-search (subseq subseq-len seq seq-len)
  (dotimes (i (- seq-len subseq-len))
    (loop for j from 0 to subseq-len do
	 (when (= j subseq-len)
	   (return-from pointer-search i))
	 (if (not (= (mem-aref subseq :unsigned-char j)
		     (mem-aref seq :unsigned-char (+ i j))))
	     (return)))))

(defun dump-pointer (p s &optional (stream t))
  (dotimes (i s)
    (format stream "~A " (mem-aref p :unsigned-char i)))
  (format stream "~%"))

(defun pointer-deref-eql (p1 p2 len)
  (dotimes (i len)
    (unless (= (mem-aref p1 :unsigned-char i) (mem-aref p2 :unsigned-char i))
      (return-from pointer-deref-eql nil))
    t))
  
(defun extract-subseq (seq old-len seq1 rem-len)
  "Return a copy of a pointer sequence with a subsequence removed."
  ;;(declare (optimize (speed 3)))
  (cond ((> rem-len old-len)
	 (error "Attempt to remove a subsequence that is longer than the original! ~A / ~A" 
		seq seq1))
	((and (= rem-len old-len) (pointer-deref-eql seq seq1 old-len))
	 (values (null-pointer) 0))
	((= rem-len old-len)
	 (values seq old-len))
	(t
	 (let ((position (pointer-search seq1 rem-len seq old-len)))
	   (if position
	       (let* ((new-len (- old-len rem-len))
		      (new-seq (foreign-alloc :unsigned-char :count new-len)))
		 (loop for i from 0 to (1- position) do
		      (setf (mem-aref new-seq :unsigned-char i) (mem-aref seq :unsigned-char i)))
		 (loop
		    for i from (+ position rem-len) to (1- old-len)
		    for j from position to (1- new-len)
		    do
		    (setf (mem-aref new-seq :unsigned-char j) (mem-aref seq :unsigned-char i)))
		 (values new-seq new-len))
	       (values seq old-len))))))

