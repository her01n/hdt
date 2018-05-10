(define-module (test inner-definitions))

(use-modules (hdt hdt)
             (test util))

(test inner-definitions
  (define a-set #f)
  (define tests
    (collect-tests
      (lambda ()
        (test the-test
          (define a (begin (set! a-set #t) 2))
          (assert (equal? a 2))))))
  (assert (equal? #f a-set) "a is not evaluated before the test run")
  (assert 
    (with-output-to-port (open-output-string) (lambda () (run-tests tests)))
    "a is defined inside the test"))

(test children-access
  (define result 
    (execute-tests
      (test parent
        (define a 7)
        (test child
          (assert (equal? a 7))))))
  (assert result "variable is accesible from child test"))

(test inner-procedure
  (define (proc arg)
    (assert (equal? arg "arg"))
    "result")
  (define (simple arg) #t)
  (assert (equal? "result" (proc "arg"))))
