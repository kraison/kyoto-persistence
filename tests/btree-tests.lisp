(in-package #:kyoto-persistence-test)

(in-suite :kyoto-persistence)

(test klist-concat-and-remove
  "Values appended under one key read back in order; removing one stores
the list without it (the old klist-test routine)."
  (let* ((path (temp-store-path "kct"))
         (tree (kp:open-btree path :duplicates-allowed? t)))
    (unwind-protect
         (progn
           (dotimes (i 1000)
             (kp:set-btree tree 1 (format nil "V-~a" i) :mode :concat))
           (let ((l (kp:get-btree tree 1 :mode :klist)))
             (unwind-protect
                  (progn
                    (is (equal "V-300" (kp:klist-nth l 300)))
                    (is-true (kp:klist-has-value? l "V-501"))
                    (kp:klist-remove l "V-900"))
               (kp:klist-free l)))
           (let ((l (kp:get-btree tree 1 :mode :klist)))
             (unwind-protect
                  (let ((items (kp:map-klist #'identity l :collect? t)))
                    (is (= 999 (length items)))
                    (is (not (member "V-900" items :test #'equal)))
                    (is (equal "V-999" (car (last items)))))
               (kp:klist-free l))))
      (ignore-errors (kp:close-btree tree))
      (when (probe-file path) (delete-file path)))))

(defmacro with-temp-btree ((var) &body body)
  "Evaluate BODY with VAR bound to a btree allowing duplicates, on a
fresh file; close and delete it afterwards."
  (let ((path (gensym "PATH")))
    `(let* ((,path (temp-store-path "kct"))
            (,var (kp:open-btree ,path :duplicates-allowed? t)))
       (unwind-protect (progn ,@body)
         (ignore-errors (kp:close-btree ,var))
         (when (probe-file ,path) (delete-file ,path))))))

(defun klist-values (tree key)
  "Every value stored under KEY in TREE, in order, or NIL if none."
  (let ((l (kp:get-btree tree key :mode :klist)))
    (when l
      (unwind-protect (kp:map-klist #'identity l :collect? t)
        (kp:klist-free l)))))

(test klist-mixed-types
  "Each element of a duplicate list is read with its own type code, not
the first element's."
  (with-temp-btree (tree)
    (let ((values (list 5 -3 7 1.5d0 "a" :kw)))
      (dolist (v values)
        (kp:set-btree tree 1 v :mode :concat))
      (is (equal values (klist-values tree 1))))))

(test klist-keeps-nil-elements
  "A NIL element does not end the list."
  (with-temp-btree (tree)
    (dolist (v (list "a" nil "b"))
      (kp:set-btree tree 1 v :mode :concat))
    (is (equal '("a" nil "b") (klist-values tree 1)))
    (let ((l (kp:get-btree tree 1 :mode :klist)))
      (unwind-protect (is (equal "b" (kp:klist-nth l 2)))
        (kp:klist-free l)))))

(test klist-matches-whole-elements
  "A value is found and removed only as a whole element, never as bytes
inside another element."
  (with-temp-btree (tree)
    (kp:set-btree tree 1 '("a" "b") :mode :concat)
    (kp:set-btree tree 1 "c" :mode :concat)
    (let ((l (kp:get-btree tree 1 :mode :klist)))
      (unwind-protect (is (null (kp:klist-has-value? l "b")))
        (kp:klist-free l)))
    (kp:rem-btree tree 1 :value "b")
    (is (equal '(("a" "b") "c") (klist-values tree 1)))))

(test klist-remove-last-value
  "Removing the only value deletes the record; draining a list leaves
nothing behind."
  (with-temp-btree (tree)
    (kp:set-btree tree 1 "only" :mode :concat)
    (kp:rem-btree tree 1 :value "only")
    (is (null (klist-values tree 1)))
    (kp:set-btree tree 2 "x" :mode :concat)
    (kp:set-btree tree 2 "y" :mode :concat)
    (kp:rem-btree tree 2 :value "x")
    (kp:rem-btree tree 2 :value "y")
    (is (null (klist-values tree 2)))))
