(define-module (test if))

(use-modules
  (hdt hdt))

(if #f
  (test 
    (assert #f "This test should not be executed")))

(when #f
  (test first
    (assert #f))
  (test second
    (assert #f)))

(define run #f)

(if #t
  (test
    (set! run #t)))

(test
  (assert run))

(define evaluated #f)

(test parent
  (if #t
    (test (assert #t))
    (test (set! evaluated #t) (assert #f))))

(test (assert (not evaluated)))
  
