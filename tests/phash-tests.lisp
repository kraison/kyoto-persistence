(in-package #:kyoto-persistence-test)

(in-suite :kyoto-persistence)

(defmacro with-temp-phash ((var) &body body)
  "Evaluate BODY with VAR bound to a phash on a fresh file; close and
delete it afterwards."
  (let ((path (gensym "PATH")))
    `(let* ((,path (temp-store-path "kch"))
            (,var (kp:open-phash ,path)))
       (unwind-protect (progn ,@body)
         (ignore-errors (kp:close-phash ,var))
         (when (probe-file ,path) (delete-file ,path))))))

(test size-slots-read-as-size-t
  "map-phash reads Kyoto's size slots as size_t; :unsigned-int read only
half of each (kyoto-persistence#2)."
  (let ((text (with-open-file
                  (in (asdf:system-relative-pathname
                       :kyoto-persistence "phash.lisp"))
                (let ((s (make-string (file-length in))))
                  (subseq s 0 (read-sequence s in))))))
    (is (null (ppcre:all-matches-as-strings
               "mem-ref [\\w-]+ :(?:int|uint32|unsigned-int)\\)" text)))))

(test phash-set-get-map
  "NUL-separated keys, :keep never overwrites, absent keys give NIL, and
map-phash sees every entry (the old phash-test routine)."
  (with-temp-phash (h)
    (let ((keys (loop for i below 10
                      collect (format nil "k~a~c~a" i (code-char 0) i))))
      (loop for k in keys
            for i from 0
            do (kp:set-phash h k i))
      (kp:set-phash h (first keys) 99 :mode :keep)
      (is (eql 0 (kp:get-phash h (first keys))))
      (is (eql 9 (kp:get-phash h (nth 9 keys))))
      (is (null (kp:get-phash h "absent")))
      (let ((seen (kp:map-phash (lambda (k v) (cons k v)) h
                                :collect? t)))
        (is (= 10 (length seen)))
        (is (equal (sort (copy-list keys) #'string<)
                   (sort (mapcar #'car seen) #'string<)))))))

(test phash-remove
  "rem-phash removes a key and leaves the others."
  (with-temp-phash (h)
    (kp:set-phash h "a" 1)
    (kp:set-phash h "b" 2)
    (kp:rem-phash h "a")
    (is (null (kp:get-phash h "a")))
    (is (eql 2 (kp:get-phash h "b")))))
