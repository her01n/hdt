(define-module (test inner-definitions))

(use-modules (hdt hdt))

(test "inner-definitions"
  (define a-set #f)
  (define test-thunk
    (lambda ()
      (test the-test
        (define a (begin (set! a-set #t) 2))
        (assert (equal? a 2)))))
  (assert (equal? #f a-set) "a is not evaluated before the test run")
  (assert 
    (with-output-to-port (open-output-string) (lambda () (execute-tests test-thunk)))
    "a is defined inside the test"))

(test "children-access"
  (define a 7)
  (test child
    (assert (equal? a 7))))

(test "inner-procedure"
  (define (proc arg)
    (assert (equal? arg "arg"))
    "result")
  (define (simple arg) #t)
  (assert (equal? "result" (proc "arg"))))
