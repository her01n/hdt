(define-module (hdt main))

; TODO organize imports

(use-modules (ice-9 ftw) (ice-9 getopt-long) (ice-9 match)
             ((srfi srfi-1) #:prefix srfi1-))

(use-modules (hdt hdt))

(define (pass . args)
  (match args
    ((path stat result) result)
    ((error path stat errno result) result)))

(define (find-files)
  (file-system-fold
    (const #t)
    (lambda (path stat result) (if (string-suffix? ".scm" path) (cons path result) result))
    pass
    pass
    pass
    pass
    '()
    "test"))

(define* (run-tests-in-files files #:key show)
  (if
    (execute-tests (lambda () (map load files)) #:show show)
    (exit 0)
    (exit 1)))
  
(define options-spec
  '((help (single-char #\h))
    (usage (single-char #\u))
    (version (single-char #\v))
    (show-output (single-char #\s))))

(define-public (test-runner)
  (define options (getopt-long (command-line) options-spec))
  (define show (option-ref options 'show-output #f))
  (cond
    ((or (option-ref options 'help #f) (option-ref options 'usage #f))
     (format #t "Automatic test framework for guile.\n")
     (format #t "Usage:\n")
     (format #t "  hdt --help\n")
     (format #t "  hdt --version\n")
     (format #t "  hdt [--show-output] [test-file ...]\n")
     (format #t "\n")
     (format #t "  hdt --help\n")
     (format #t "    Display this help and exit.\n\n")
     (format #t "  hdt --version\n")
     (format #t "    Display program version and exit.\n\n")
     (format #t "  hdt [--show-output] [test-file ...]\n")
     (format #t "    Execute tests in given file(s).\n")
     (format #t "    If no files are given, execute tests in all files in 'test' directory.\n")
     (format #t "    Return success if all test passes, failure otherwise.\n")
     (format #t "\n")
     (format #t "  --show-output, -s\n")
     (format #t "    Display output from passed tests.\n")
     (format #t "    By default the output from tests that are passing is suppressed,\n")
     (format #t "    only output from failed test is displayed.\n"))
    ((option-ref options 'version #f)
     (format #t "hdt version 0.1\n"))
    ((pair? (option-ref options '() '()))
     (run-tests-in-files (option-ref options '() '()) #:show show))
    (else
     (run-tests-in-files (find-files) #:show show))))


