(define-module (test hook))

(use-modules (hdt hdt)
             (test util))

(test hook
  (define log "")
  (execute-tests
    (test
      (hook
        (set! log (string-append log "hook ")))
      (set! log (string-append log "test "))))
  (assert (equal? "test hook " log)))

(test hooks
  (define log "")
  (execute-tests
    (test
      (hook
        (set! log (string-append log "first ")))
      (hook
        (set! log (string-append log "second ")))))
  (assert (equal? "second first " log)))

(test hook-run-assert-failed
  (define hook-run #f)
  (execute-tests
    (test
      (hook (set! hook-run #t))
      (assert #f)))
  (assert hook-run))

(test hook-run-test-error
  (define hook-run #f)
  (execute-tests
    (test
      (hook (set! hook-run #t))
      (error "test-error")))
  (assert hook-run))

(test hook-run-previous-hook-error
  (define hook-run #f)
  (execute-tests
    (test
      (hook (set! hook-run #t))
      (hook (error "test-error"))))
  (assert hook-run))

(test hook-error-report
  (define output
    (with-output-to-string
      (lambda ()
        (run-tests
          (collect-tests
            (lambda () (test (hook (error "test-error")))))))))
  (assert (string-contains output "test-error")))
