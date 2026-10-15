(in-package #:kyoto-persistence-test)

(in-suite :kyoto-persistence)

(defun foreign-octets (&rest bytes)
  "A malloc'd buffer holding BYTES; free it with foreign-free."
  (let ((p (cffi:foreign-alloc :unsigned-char :count (length bytes))))
    (loop for b in bytes
          for i from 0
          do (setf (cffi:mem-aref p :unsigned-char i) b))
    p))

(defun buffer-octets (p n)
  "The first N bytes at P, as a list."
  (loop for i below n collect (cffi:mem-aref p :unsigned-char i)))

(test pointer-search
  "Finds a subsequence's offset, including one ending at the last byte
(the old tp routine, plus that edge)."
  (let ((seq (foreign-octets 0 1 2 3 4 5 6 7 8 9))
        (mid (foreign-octets 1 2 3 4))
        (tail (foreign-octets 7 8 9))
        (absent (foreign-octets 4 3)))
    (unwind-protect
         (progn
           (is (eql 1 (kp::pointer-search mid 4 seq 10)))
           (is (eql 7 (kp::pointer-search tail 3 seq 10)))
           (is (null (kp::pointer-search absent 2 seq 10))))
      (mapc #'cffi:foreign-free (list seq mid tail absent)))))

(test extract-subseq
  "Removes the first occurrence of a subsequence (the old tp2 routine)."
  (let ((seq (foreign-octets 0 1 2 3 4 5 6 7 8 9))
        (cut (foreign-octets 1 2 3 4)))
    (unwind-protect
         (multiple-value-bind (new len) (kp::extract-subseq seq 10 cut 4)
           (is (eql 6 len))
           (is (equal '(0 5 6 7 8 9) (buffer-octets new len)))
           (kyoto-cabinet-ffi:kcfree new))
      (mapc #'cffi:foreign-free (list seq cut)))))
