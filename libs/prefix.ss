
;; assert helpers

(define assert-pass-display? #t)

(define (assert-pass expr)
  (cond
    (assert-pass-display?
      (display "[A] PASS ")
      (display expr)
      (newline))
    (else (void))))

(define (assert-fail expr)
  (display "[A] FAIL ")
  (display expr)
  (newline))

(define (assert-fail-3 expr a b)
  (display "[A] FAIL ")
  (display expr)
  (display ": ")
  (display a)
  (display " ")
  (display b)
  (newline))

(define-syntax (assert-tr stx)
  (syntax-case stx ()
    ((name expr)
     #'(cond
         (expr (assert-pass '(name expr)))
         (else (assert-fail '(name expr)))))))

(define-syntax (assert-nt stx)
  (syntax-case stx ()
    ((name expr)
     #'(cond
         (expr (assert-fail '(name expr)))
         (else (assert-pass '(name expr)))))))

(define-syntax (assert-eq stx)
  (syntax-case stx ()
    ((name expr-a expr-b)
     #'(let ((x expr-a) (y expr-b))
         (cond
           ((eq? x y) (assert-pass '(name expr-a expr-b)))
           (else (assert-fail-3 '(name expr-a expr-b) x y)))))))

(define-syntax (assert-nq stx)
  (syntax-case stx ()
    ((name expr-a expr-b)
     #'(let ((x expr-a) (y expr-b))
         (cond
           ((eq? x y) (assert-fail '(name expr-a expr-b)))
           (else (assert-pass '(name expr-a expr-b))))))))

;; (define-property <variable> <property> <value>)
;;
;; Attaches a property to an existing identifier binding without disturbing
;; the existing meaning of the identifier in the scope of that binding. It
;; is typically used by one macro to record information about a binding for
;; use by another macro. Both <variable> and <property> must be identifiers.
;;
;; When defining sets of dependent macros, it is often convenient to attach
;; information to identifiers in the same compile time environment that the
;; expander uses to record infromation about variables, keywords, module
;; names, etc. ChezScheme provides two mechanisms for attaching information
;; to identifiers in the compile-time environment: compile-time values and
;; compile-time properties.
;;
;; The mechanisms used by a macro to obtain compile-time values and properties
;; are similar. In both cases, the macro's transformer returns a procedure p
;; rather than a syntax object. The expander invokes p with one argument, an
;; environment-lookup procedure lookup, which p can then use to obtain
;; compile-time values and properties for one or more identifiers before it
;; constructs the macro's final output. lookup accepts one or two identifier
;; arguments. With one argument, id, lookup returns the compile-time value
;; of id, or #f if id has no compile-time value. With two arguments, id and
;; key, lookup returns the value of id's key property, or #f if id has no key
;; property.

(define type)

(define-syntax get-property
  (lambda (stx)
    (lambda (lookup)
      (syntax-case stx ()
        ((_ var prop)
         #`'#,(datum->syntax #'var (lookup #'var #'prop)))))))

(define (get-type a)
  (get-property a type))

(define (get-type-string a)
  (symbol->string (get-type a)))

(define-syntax (test-property stx)
  (syntax-case stx ()
    ((_) #'(let ((x 1024))
             (define-property x type 'number)
             (assert-eq 'number (get-property x type))))))

;; syntax types
;; constant - string, number, character, boolean, bytevector
;; identifier symbol list vector pair unknown

(meta define (syntax-type-class stx)
  (let ((d (syntax->datum stx)))
    (cond
      ((identifier? stx) 'identifier)
      ((string? d) 'constant)
      ((number? d) 'constant)
      ((char? d) 'constant)
      ((boolean? d) 'constant)
      ((bytevector? d) 'constant)
      ((vector? d) 'vector)
      ((null? d) 'list)
      ((list? d)
         (let ((1st (car d))
               (2nd (if (null? (cdr d)) (void) (cadr d))))
           (if (or (eq? 1st 'quote) (eq? 1st 'quasiquote))
               (cond
                 ((eq? 2nd (void)) 'list)
                 ((identifier? (datum->syntax #'1st 2nd)) 'symbol)
                 ((string? 2nd) 'constant)
                 ((number? 2nd) 'constant)
                 ((char? 2nd) 'constant)
                 ((boolean? 2nd) 'constant)
                 ((bytevector? 2nd) 'constant)
                 (else 'list)))
               'list)))
      ((pair? d) 'pair)
      (else 'unknown)))

(define-syntax (test-syntax-type-class stx)
      (syntax-case stx ()
        ((_ a) (let* ((s #'a)
                      (d (syntax->datum s)))
                 (write s) (newline)
                 (write d) (newline)
                 (write (syntax-type-class s))
                 #'(newline)))))

;; string operations

(define (empty-string? s)
  (eq? 0 (string-length s)))

(define (split s c)   ;; split a string into two parts, c must be a character
  (let ((len (string-length s)))
    (let find ((n 0))
      (if (< n len)
          (if (eq? c (string-ref s n))
              (values (substring s 0 n) (substring s (+ n 1) len))
              (find (+ n 1)))
          (values s "")))))

(define (string-id stx)
  (symbol->string (syntax->datum stx)))

(define (string->syntax stx str)
  (datum->syntax stx (string->symbol str)))

(meta define (split-id stx sep)
  (symbol->string (syntax->datum stx)

;; dot access style

(define-syntax (dot stx)
  (syntax-case stx ()
    ((_ (name arg ...) ...)
     (let-values (((var-name func-name) (split (string-id #'name) #\.)))
       (cond
         ((empty-string? func-name) stx)
         (else
           (with-syntax* ((var (string->syntax #'name var-name))
                          (real-func (string->syntax #'name (format "~a-~a" (get-type-string var) func-name))))
             #'(begin
                 (real-func var arg ...)
                 ...))))))))

