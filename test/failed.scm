(define-module (test failed))

(use-modules (hdt hdt) (test util))

(test "failed-false"
  (define result
    (with-output-to-port (open-output-string)
      (lambda () (execute-tests (lambda () (test "failing" (assert #f)))))))
  (assert (not result) "run with failed tests returns #f"))

(test "failed-test-output"
  (define output
    (execute-tests-with-output-to-string
      (test "failing" (assert (equal? 0 1) "message"))))
  (assert (string-contains output "failing") "name of the failing test")
  (assert (string-contains output "message") "assertion message")
  (assert (string-contains output "test/failed.scm:14") "assertion location")
  (assert (string-contains output "(equal? 0 1)") "assertion expression")
  (assert (string-contains output "failed") "'failed' string"))

(test "expand-equal"
  (define output
    (execute-tests-with-output-to-string
      (test "equal"
        (define a "actual")
        (assert (equal? "expected" a)))))
  (assert (string-contains output "\"actual\"") "equal? arguments are expanded")
  (assert (string-contains output "(equal? \"expected\" a)") "original expression is also displayed"))

(test "expand-string-contains"
  (define output
    (execute-tests-with-output-to-string
      (test "string-contains"
        (define h "hay")
        (assert (string-contains h "needle")))))
  (assert (string-contains output "hay") "string-contain arguments are expanded"))

(test "expand-not"
  (define output
    (execute-tests-with-output-to-string
      (test "not"
        (define value "hey")
        (assert (not value)))))
  (assert (string-contains output "hey") "not argument is expanded"))

(test "expand-member"
  (define output
    (execute-tests-with-output-to-string
      (test "member"
        (define f "foo")
        (assert (member f '("bar" "baz"))))))
  (assert (string-contains output "foo") "member's first argument is expanded"))

