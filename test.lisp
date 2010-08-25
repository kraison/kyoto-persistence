(require 'asdf)

(asdf:oos 'asdf:load-op 'kyoto-persistence)

(in-package #:kyoto-persistence)

(defun klist-test ()
  (let ((btree (open-btree "/var/tmp/btree.kct" :duplicates-allowed? t))
	(key1 1))
    (unwind-protect
	 (progn
	   (dotimes (i 10000)
	     (set-btree btree key1 (format nil "V-1-~A" i) :mode :concat))
	   (let ((l (get-btree btree key1 :mode :klist)))
	     ;;(dump-pointer (klist-pointer l) (klist-size l))
	     (format t "~A: ~A~%" 300 (klist-nth l 300))
	     (format t "has-value V-1-5001? ~A~%" (klist-has-value? l "V-1-5001"))
	     (dotimes (i 10000)
	       (klist-next l)
	       (when (= 0 (mod i 1000))
		 (format t "GOT ~A~%" (klist-nth l i))))
	     (time (klist-remove l "V-1-9000"))
	     (dotimes (i 10000)
	       (klist-next l)
	       (when (= 0 (mod i 1000))
		 (format t "GOT ~A~%" (klist-nth l i))))
	     (map-klist #'(lambda (i)
			    (format t "~A : " i))
			l)
	     (when (klist? l) (klist-free l))))
      (progn
	(close-btree btree)
	(delete-file "/var/tmp/btree.kct")))))

(defun phash-test ()
  (let ((hash (open-phash "/var/tmp/hash.kch")))
    (unwind-protect
	 (let ((words '("one" "two" "three" "four" "five")))
	   (dotimes (i 10)
	     (set-phash hash 
			(format nil "~A~A~A" (nth (random 5) words) #\Nul (nth (random 5) words))
			(format nil "~A" i)
			:mode :keep))
	   (map-phash #'(lambda (key val)
			  (let ((pieces (split key '(#\Nul))))
			    (format t "Got key pieces: ~A for ~A~%" pieces val)))
		      hash))
      (progn
	(close-phash hash)
	(delete-file "/var/tmp/hash.kch")))))

(defun split (string &optional (ws '(#\Space #\Tab)) max)
  (flet ((is-ws (char) (find char ws)))
    (nreverse
     (let ((list nil) (start 0) (words 0) end)
       (loop
  (when (and max (>= words (1- max)))
    (return (cons (subseq string start) list)))
  (setf end (position-if #'is-ws string :start start))
  (push (subseq string start end) list)
  (incf words)
  (unless end (return list))
  (setf start (1+ end)))))))

(defun tp ()
  (let ((p1 (foreign-alloc :unsigned-char :count 10 :initial-element 0))
	(p2 (foreign-alloc :unsigned-char :count 4 :initial-element 0)))
    (dotimes (i 10)
      (setf (mem-aref p1 :unsigned-char i) i))
    (setf (mem-aref p2 :unsigned-char 0) 1)
    (setf (mem-aref p2 :unsigned-char 1) 2)
    (setf (mem-aref p2 :unsigned-char 2) 3)
    (setf (mem-aref p2 :unsigned-char 3) 4)
    (format t "Comparing~%") 
    (dump-pointer p1 10) (dump-pointer p2 4)
    (prog1
	(pointer-search p2 4 p1 10)
      (foreign-free p1)
      (foreign-free p2))))

(defun tp2 ()
  (let ((p1 (foreign-alloc :unsigned-char :count 10 :initial-element 0))
	(p2 (foreign-alloc :unsigned-char :count 4 :initial-element 0)))
    (dotimes (i 10)
      (setf (mem-aref p1 :unsigned-char i) i))
    (setf (mem-aref p2 :unsigned-char 0) 1)
    (setf (mem-aref p2 :unsigned-char 1) 2)
    (setf (mem-aref p2 :unsigned-char 2) 3)
    (setf (mem-aref p2 :unsigned-char 3) 4)
    (format t "Comparing~%") 
    (dump-pointer p1 10) (dump-pointer p2 4)
    (prog1
	(multiple-value-bind (new-seq new-len) (extract-subseq p1 10 p2 4)
	  (format t "New seq is length ~A: " new-len)
	  (dump-pointer new-seq new-len))
      (foreign-free p1)
      (foreign-free p2))))

