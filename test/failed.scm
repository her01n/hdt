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

(define (my-equal? a b)
  (equal? a b))

(test "expand-function-arguments"
  (define output
    (execute-tests-with-output-to-string
      (test
        (define a "value-a")
        (define b "value-b")
        (assert (my-equal? a b)))))
  (assert (string-contains output "value-a")))

(test "expand-not"
  (define output
    (execute-tests-with-output-to-string
      (test "not"
        (define value "hey")
        (assert (not value)))))
  (assert (string-contains output "hey") "not argument is expanded"))

(test "expand-macro-arguments"
  (define output
    (execute-tests-with-output-to-string
      (test
        (define a "value-a")
        (assert (and a #f)))))
  (assert (string-contains output "value-a")))

(test "do-not-evaluate-arguments"
  (define called #f)
  (define (do-not-call) (set! called #t))
  (define output
    (execute-tests-with-output-to-string
      (test
        (assert (and #f (do-not-call))))))
  (assert (not called)))

