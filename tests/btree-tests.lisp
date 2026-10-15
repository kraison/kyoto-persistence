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
