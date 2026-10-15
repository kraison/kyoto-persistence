(in-package #:kyoto-persistence-test)

(in-suite :kyoto-persistence)

(test serialize-round-trips
  "Every supported type survives serialize then deserialize."
  (dolist (x (list "hello" "" "naïve 日本" 0 1 255 256 (expt 2 70)
                   (- (expt 2 70)) 1/3 -7/2 1.5f0 -2.25d0 #\a #\λ t nil
                   :key 'kp:open-store '(1 "two" 3.0d0) '(1 . 2)))
    (is (equal x (round-trip x)) "~s did not round-trip" x)))

(test negative-integers
  "Negative integers keep their magnitude; -1 and negative powers of 256
were encoded with too few bytes (kyoto-persistence#2)."
  (dolist (x (list -1 -2 -255 -256 -65536 (- (expt 2 64)) -256/3))
    (is (eql x (round-trip x)) "~s came back as ~s" x (round-trip x))))

(test uuid-round-trip
  (let ((u (uuid:make-v4-uuid)))
    (is (uuid:uuid= u (round-trip u)))))

(test timestamp-round-trip
  "Timestamps keep their nanoseconds."
  (let ((ts (local-time:now)))
    (is (local-time:timestamp= ts (round-trip ts)))))
