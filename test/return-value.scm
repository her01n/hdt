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
  (let* ((pipe (open-input-pipe "readlink -f ."))
         (dir (get-line pipe))
         (status 
           (system
             (string-append 
               "cd /tmp/hdt; env GUILE_LOAD_PATH=" dir " " 
               dir "/bin/hdt 2>&1 >/dev/null"))))
    (assert (not (equal? 0 (status:exit-val status))))
    (close-pipe pipe)))

