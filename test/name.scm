(define-module (test name))

(use-modules (srfi srfi-1))

(use-modules 
  (hdt hdt)
  (test util))

(test string-name
  (define output
    (execute-tests-with-output-to-string
      (test "the name" (assert #f))))
  (assert (string-contains output "the name")))

(test symbol-name
  (define output
    (execute-tests-with-output-to-string
      (test the-name (assert #f))))
  (assert (string-contains output "the-name")))

(test anonymous
  (define output
    (execute-tests-with-output-to-string
      (test (assert #f))))
  (assert (string-contains output "failed")))

(test nested
  (define output
    (execute-tests-with-output-to-string
      (test parent (test child (assert #f)))))
  (assert (string-contains output "parent/child")))

