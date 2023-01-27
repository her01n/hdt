(define-module (test throws))

(use-modules (hdt hdt) (test util))

(test "throws-exception" 
  (test "pass"
    (assert (throws-exception (/ 1 0)))
    (assert (not (throws-exception (+ 1 0)))))
  (test "failure"
    (assert
      (not
        (with-output-to-port (open-output-string)
          (lambda ()
            (execute-tests
              (lambda ()
                (test (assert (throws-exception (+ 1 1)))))))))))
  (test "display-value"
    (define output
      (execute-tests-with-output-to-string
        (test (assert (throws-exception (+ 1 1))))))
    (assert (string-contains output "2"))))

