(define-module (hdt hdt)
  #:export (execute-tests test-runner)
  #:export-syntax (assert test hook throws-exception))

(use-modules (ice-9 ftw) (ice-9 getopt-long) (ice-9 match)
             ((srfi srfi-1) #:prefix srfi1-))

(define (execute-tests thunk)
  (run-tests (collect-tests thunk)))

(define (collect-tests thunk)
  (set! tests (cons '() tests))
  (with-fluid* test-name '() thunk)
  (let ((collected (car tests)))
    (set! tests (cdr tests))
    (reverse collected)))

(define tests '(() ()))

(define (register-test test-name proc)
  (define name-string
    (if (srfi1-every not test-name)
      #f
      (string-join (map (lambda (name) (or name "")) (reverse test-name)) "/")))
  (define (the-test arg)
    (cond
      ((equal? arg 'name) name-string)
      ((equal? arg 'execute) (proc))))
  (set! tests (cons (cons the-test (car tests)) (cdr tests))))

(define test-proc (make-fluid (lambda () #f)))
(define test-name (make-fluid '()))

(define-syntax test'	
  (syntax-rules (define test)
    ((test' (define (proc-name args ...) proc-expr ...) test-expr ...)
      (let ((proc-name #f)
            (proc-proc (lambda (args ...) proc-expr ...)))
        (test' (set! proc-name proc-proc) test-expr ...)))
    ((test' (define var-name var-expr) expr ...)
      (let ((var-name #f)
            (evaluate (lambda () var-expr)))
        (test' (set! var-name (evaluate)) expr ...)))
    ((test')
      (register-test (fluid-ref test-name) (fluid-ref test-proc)))
    ((test' (test inner-expr ...) outer-expr ...)
      (begin
        (test inner-expr ...)
        (test' outer-expr ...)))
    ((test' expr tail ...)
      (let ((parent-proc (fluid-ref test-proc)))
        (with-fluid* test-proc (lambda () (parent-proc) expr)
          (lambda () (test' tail ...)))))))

(define (datum->name expr)
  (cond
    ((string? expr) expr)
    ((symbol? expr) (symbol->string expr))
    (else #f)))

(define-syntax test
  (lambda (x)
    (syntax-case x ()
      ((test expr tail ...)
        (let ((name (datum->name (syntax->datum (syntax expr)))))
          (if name
            #`(with-fluid* test-name (cons #,name (fluid-ref test-name))
                (lambda () (test' tail ...)))
            #`(with-fluid* test-name (cons #f (fluid-ref test-name))
                (lambda () (test' expr tail ...)))))))))
      
(define-syntax hook
  (syntax-rules ()
    ((hook expr ...)
      (register-hook (lambda () expr ...)))))

(define (n-tests n)
  (if (equal? n 1)
    "1 test"
    (format #f "~a tests" n)))

(define hooks (make-fluid '()))

(define (register-hook hook)
  (fluid-set! hooks (cons hook (fluid-ref hooks))))

(define (report failure)
  (match failure
    ((backtrace 'assertion-failed (message filename line exprs))
      (format #t "assertion~a failed:\n" (if message (string-append " \"" message "\"") ""))
      (for-each (lambda (expr) (format #t "  ~a\n" expr)) exprs)
      (format #t "~a:~a\n" filename line)
      (display "\n"))
    ((backtrace key args)
      (format #t "error: ~a ~a\n" key args)
      (format #t "backtrace:\n~a\n" backtrace)
      (display "\n"))))

(define (run-tests tests)
  (define failures
    (map
      (lambda (test)
        (with-fluid* hooks '()
          (lambda ()
            (define test-error (catch-error (lambda () (test 'execute))))
            (define hook-errors (map catch-error (fluid-ref hooks)))
            (define errors (filter identity (cons test-error hook-errors)))
            (if (null? errors) (display ".") (display "F"))
            errors)))
      tests))
  (define hook-failures (filter identity (map catch-error (fluid-ref hooks))))
  (fluid-set! hooks '())
  (display "\n")
  (if (and (srfi1-every null? failures) (null? hook-failures))
    (begin
      (format #t "~a ok\n" (n-tests (length tests)))
      #t)
    (begin
      (for-each
        (lambda (test failures)
          (if (not (null? failures))
            (begin
              (format #t "Test ~a failed.\n" (test 'name))
              (for-each report failures)))) 
        tests failures)
      (for-each
        (lambda (failure)
          (format #t "Error in hook.\n")
          (report failure))
        hook-failures)
      (format #t "~a failed.\n" (n-tests (srfi1-count (compose not null?) failures)))
      #f)))

(define (catch-error thunk)
  (call/cc
    (lambda (return-error)
      (define outer (stack-length (make-stack #t)))
      (with-throw-handler #t
        (lambda () (thunk) #f)
        (lambda (key . args)
          (define stack (make-stack #t 1 (+ outer 1)))
          (define backtrace
            (call-with-output-string (lambda (output) (display-backtrace stack output))))
          (return-error (list backtrace key args))))
      #f)))

(define (execute-check message filename line expr proc . args)
  (if (not (apply proc args))
    (let ((proc (procedure-name proc))
          (args (string-join (map (lambda (arg) (format #f "~s" arg)) args) " ")))
      (throw 'assertion-failed message filename line (list expr (format #f "(~a ~a)" proc args))))))

(define (execute-throws-check message filename line expr thunk)
  (define value
    (catch #t
      (lambda () (thunk))
      (lambda (key . args) 'hdt-exception)))
  (if (not (equal? 'hdt-exception value))
    (throw 'assertion-failed message filename line (list expr (format #f "(throws-exception ~a)" value)))))

; TODO expand all procedures - can i tell what is a procedure and what an expanded macro?
; does it matter?

(define-syntax assert
  (lambda (x)
    (define source (syntax-source x))
    (define filename (assoc-ref source 'filename))
    (define line (+ 1 (assoc-ref source 'line)))
    (syntax-case x ()
      ((assert check message)
        (let ((expr (with-output-to-string (lambda () (write (syntax->datum (syntax check)))))))
          (syntax-case (syntax check) (equal? string-contains not throws-exception member srfi1-member)
            ((equal? arg1 arg2)
              #`(execute-check message #,filename #,line #,expr equal? arg1 arg2))
            ((string-contains arg1 arg2)
              #`(execute-check message #,filename #,line #,expr string-contains arg1 arg2))
            ((not arg)
              #`(execute-check message #,filename #,line #,expr not arg))
	    ((throws-exception arg)
	      #`(execute-throws-check message #,filename #,line #,expr (lambda () arg)))
            ((member arg1 arg2)
              #`(execute-check message #,filename #,line #,expr member arg1 arg2))
            ((srfi1-member arg1 arg2)
              #`(execute-check message #,filename #,line #,expr srfi1-member arg1 arg2))
            (check
              #`(if (not check) (throw 'assertion-failed message #,filename #,line (list #,expr)))))))
      ((assert check) #`(assert check #f)))))

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

(define (run-tests-in-files files)
  (if
    (run-tests
      (collect-tests
        (lambda () (map load-from-path files))))
    (exit 0)
    (exit 1)))
  
(define options-spec
  '((help (single-char #\h))
    (usage (single-char #\u))
    (version (single-char #\v))))

(define (test-runner)
  (define options (getopt-long (command-line) options-spec))
  (cond
    ((or (option-ref options 'help #f) (option-ref options 'usage #f))
     (format #t "Automatic test framework for guile.\n")
     (format #t "Usage:\n")
     (format #t "  hdt --help\n")
     (format #t "  hdt --version\n")
     (format #t "  hdt [test-file ...]\n")
     (format #t "\n")
     (format #t "  hdt --help\n")
     (format #t "    Display this help and exit.\n\n")
     (format #t "  hdt --version\n")
     (format #t "    Display program version and exit.\n\n")
     (format #t "  hdt [test-file ...]\n")
     (format #t "    Execute tests in given file(s).\n")
     (format #t "    If no files are given, execute tests in all files in 'test' directory.\n")
     (format #t "    Return success if all test passes, failure otherwise.\n"))
    ((option-ref options 'version #f)
     (format #t "hdt version 0.1\n"))
    ((pair? (option-ref options '() '()))
     (run-tests-in-files (option-ref options '() '())))
    (else
     (run-tests-in-files (find-files)))))

; just for completeness.
; it is not used in normal usage: (assert (throws-exception ...))
(define-syntax throws-exception
  (syntax-rules ()
    ((throws-exception expr)
      (catch #t
        (lambda () expr #f)
        (lambda (key . args) #t)))))

