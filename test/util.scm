(define-module (test util)
  #:export-syntax (execute-tests))

(use-modules (hdt hdt))

(define-syntax execute-tests
  (syntax-rules ()
    ((execute-tests tests ...)
      (with-output-to-port (open-output-string)
        (lambda ()
          (run-tests (collect-tests (lambda () tests ...))))))))
