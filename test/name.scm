(define-module (test name))

(use-modules (srfi srfi-1))

(use-modules (hdt hdt))

(test "string-name"
  (define test (car (collect-tests (lambda () (test "the name" #f)))))
  (assert (equal? "the name" (test 'name))))

(test "symbol-name"
  (define test (car (collect-tests (lambda () (test the-name #f)))))
  (assert (equal? "the-name" (test 'name))))

(test "anonymous"
  (define test (car (collect-tests (lambda () (test #f)))))
  (assert (equal? #f (test 'name))))

(test "nested"
  (define tests (collect-tests (lambda () (test "parent" (test "child" #f)))))
  (assert (equal? "parent/child" ((first tests) 'name)))
  (assert (equal? "parent" ((second tests) 'name))))
    