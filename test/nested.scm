(define-module (test nested))

(use-modules (hdt hdt) (test util))

(test run-nested-tests
  (define log "")
  (execute-tests-with-output-to-string
    (test "root"
      (set! log (string-append log "root "))
      (test "left"
        (set! log (string-append log "left ")))
      (set! log (string-append log "center "))
      (test "right"
        (set! log (string-append log "right ")))
      (set! log (string-append log "finish "))))
  (assert (equal? "root left root center right root center finish " log) "nested tests are executed"))

