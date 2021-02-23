(define-module (hdt hdt)
  #:export (execute-tests)
  #:export-syntax (assert test hook throws-exception))

(use-modules (ice-9 ftw) (ice-9 getopt-long) (ice-9 match)
             ((srfi srfi-1) #:prefix srfi1-))

(define* (execute-tests thunk #:key show)
  (run-tests (collect-tests thunk) #:show show))

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
      (for-each (lambda (expr) (format #t "  ~s\n" expr)) exprs)
      (format #t "~a:~a\n" filename line)
      (display "\n"))
    ((backtrace key args)
      (format #t "error: ~a ~a\n" key args)
      (format #t "backtrace:\n~a\n" backtrace)
      (display "\n"))))

; run a single test, including hooks and return the errors
(define* (run-test test #:key show)
  (define (run)
    (define test-error (catch-error (lambda () (test 'execute))))
    (define hook-errors (map catch-error (fluid-ref hooks)))
    (define errors (filter identity (cons test-error hook-errors)))
    errors)
  (define (hide-output)
    (define output (open-output-string))
    (define errors (with-output-to-port output run))
    (if (not (null? errors)) (display (get-output-string output)))
    ; XXX Under some conditions (multi thread),
    ; standard function may write to a port that is no longer the current output port.
    ; We rely on garbage collection to close the port.
    errors)
  (if show
    (run)
    (hide-output)))

(define* (run-tests tests #:key show)
  (define failures
    (map
      (lambda (test)
        (with-fluid* hooks '()
          (lambda ()
            (define errors (run-test test #:show show))
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

(define (execute-check message filename line thunk description)
  (if (not (thunk))
    (throw 'assertion-failed message filename line description)))

(define-syntax lambdas
  (syntax-rules ()
    ((lambdas form values) form)
    ((lambdas (form* ...) values arg arg* ...)
     (lambdas
       (form* ... ((lambda () (define value arg) (list-set! values 0 value) value)))
       (cdr values)
       arg* ...))))

(define-syntax assert
  (lambda (x)
    (define source (syntax-source x))
    (define filename (assoc-ref source 'filename))
    (define line (+ 1 (assoc-ref source 'line)))
    (syntax-case x ()
      ((assert (proc arg* ...) message)
       #`((lambda ()
            (define values (map (const '*not-evaluated) '(arg* ...)))
            (execute-check
              message #,filename #,line
              (lambda () (lambdas (proc) values arg* ...))
              (list (quote (proc arg* ...)) (cons (quote proc) values))))))
      ((assert check message)
       #`(execute-check
           message #,filename #,line
           (lambda () check)
           (list (quote check))))
      ((assert check) #`(assert check #f)))))

(define-syntax throws-exception
  (syntax-rules ()
    ((throws-exception expr)
      (catch #t
        (lambda () expr #f)
        (lambda (key . args) #t))))) 

