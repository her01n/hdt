(define-module (test hdt-file))

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
  (with-output-to-file "tmp/success.scm"
    (lambda ()
      (format #t "(use-modules (hdt hdt))\n")
      (format #t "(test success (format #t \"success!\\n\"))")))
  (let* ((pipe (open-input-pipe "cd tmp; ../bin/hdt success.scm"))
         (output (get-string-all pipe)))
    (assert (string-contains output "success!"))
    (close-port pipe)))

