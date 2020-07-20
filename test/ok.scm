(define-module (test ok))

(use-modules (hdt hdt)
             (test util))

(test success
  (assert
    (with-output-to-port (open-output-string)
      (lambda () (execute-tests (lambda () (test #f)))))))

(test two-tests-ok
  (define output
    (execute-tests-with-output-to-string
      (test #f)
      (test #f)))
  (assert (string-contains output "2 tests ok")))

