(define-module (test return-value))

(use-modules 
  (hdt hdt)
  (ice-9 popen)
  (rnrs io ports))

(test
  (system "rm -rf tmp")
  (system "mkdir -p tmp/test")
  (with-output-to-file "tmp/test/failure.scm"
    (lambda ()
      (format #t "(use-modules (hdt hdt))\n")
      (format #t "(test fails (assert #f))")))
  (let* ((status 
          (system
            (string-append "cd tmp; ../bin/hdt 2>&1 >/dev/null"))))
    (assert (not (equal? 0 (status:exit-val status))))))
