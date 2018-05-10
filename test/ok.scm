(define-module (test ok))

(use-modules (hdt hdt)
             (test util))

(define empty-test
  (car (collect-tests (lambda () (test "empty" #f)))))

(test "success"
  (define result
    (with-output-to-port (open-output-string)
      (lambda ()
        (run-tests (list empty-test)))))
  (assert result "success run returns true value"))
  
(test "two-tests-ok"
  (define output
    (with-output-to-string (lambda () (run-tests (list empty-test empty-test)))))
  (assert (string-contains output "2 tests ok")))
  