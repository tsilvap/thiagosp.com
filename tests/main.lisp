(defpackage thiagosp/tests/main
  (:use :cl
        :thiagosp
        :rove))
(in-package :thiagosp/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :thiagosp)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
