(define-module (test nested))

(use-modules (hdt hdt))

(define log "")

(define nested-tests
  (collect-tests
    (lambda ()
      (test "root"
        (set! log (string-append log "root "))
        (test "left"
          (set! log (string-append log "left ")))
        (set! log (string-append log "center "))
        (test "right"
          (set! log (string-append log "right ")))
        (set! log (string-append log "finish "))))))

(test "run-nested-tests"
  (set! log "")
  (with-output-to-string
    (lambda () (run-tests nested-tests)))
  (assert (equal? "root left root center right root center finish " log) "nested tests are executed"))
