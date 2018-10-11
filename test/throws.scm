(define-module (test throws))

(use-modules (hdt hdt))

(test throws 
  (test pass
    (assert (throws-exception (/ 1 0))))
  (test failure
    (assert
      (not 
        (with-output-to-port (open-output-string)
          (lambda ()
            (run-tests (collect-tests (lambda () (test (assert (throws-exception (+ 1 1))))))))))))
  (test display-value
    (define output
      (with-output-to-string
        (lambda () (run-tests (collect-tests (lambda () (test (assert (throws-exception (+ 1 1))))))))))
    (assert (string-contains output "2"))))

