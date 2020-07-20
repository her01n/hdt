(define-module (test util)
  #:export-syntax (execute-tests-with-output-to-string))

(use-modules (hdt hdt))

(define-syntax execute-tests-with-output-to-string
  (syntax-rules ()
    ((execute-tests-with-output-to-string tests ...)
     (with-output-to-string
       (lambda ()
         (execute-tests
           (lambda () tests ...)))))))

