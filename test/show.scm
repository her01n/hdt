(define-module (test show))

(use-modules (hdt hdt))

(test silent
  (define output
    (with-output-to-string
       (lambda ()
         (execute-tests
           (lambda ()
             (test success
               (display "success\n"))
             (test failure
               (display "failure\n")
               (assert #f)))))))
  (assert (not (string-contains output "success")))
  (assert (string-contains output "failure")))

(test show
  (define output
    (with-output-to-string
      (lambda ()
        (execute-tests
          (lambda ()
            (test success
              (display "success\n")))
          #:show #t))))
  (assert (string-contains output "success")))
  
