
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

(define-syntax (set-object-prop stx)
  (syntax-case stx ()
    ((_ var prop value)
     (and (identifier? #'var) (identifier? #'prop) (not (identifier? #'value)))
     (with-syntax ((id (

(define-syntax (get-object-prop stx)
  (lambda (lookup)
    (syntax-case stx ()
      ((_ var prop)
       #`'#,(datum->syntax #'var (lookup #'var #'prop))))))

(define type)

(define-syntax (get-object-type stx)
  (syntax-case stx ()
    ((_ var) #'(get-object-prop var type))))

(define-syntax (get-type-string stx)
  (syntax-case stx ()
    ((_ var) #'(symbol->string (get-type var)))))

(define-syntax (attach-string-type stx)
  (syntax-case stx ()
    ((_ var) #'(define-property var type 'string))))

(define-syntax (object-type-string-impl stx)
  (syntax-case stx ()
    ((name str)
     (with-syntax ((identifier (string->identifier #'name #'str)))
       #'(get-type-string identifier)))))

(define (object-type-string obj)
  (cond
    ((identifier? #'obj) (get-type-string obj))
    ((string? obj) (object-type-string-impl obj))
    (else (object-type-string-impl (syntax->datum obj)))))

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

(define (string-empty? s)
  (eq? 0 (string-length s)))

(define (string-split str ch)   ;; split a string into two parts, ch must be a character
  (let ((len (string-length str)))
    (let find ((n 0))
      (if (< n len)
          (if (eq? ch (string-ref str n))
              (values (substring str 0 n) (substring str (+ n 1) len))
              (find (+ n 1)))
          (values str "")))))

;; syntax helpers

(define (string->identifier ctx str)
  (if (string? str)
      (datum->syntax ctx (string->symbol str))
      (datum->syntax ctx (string->symbol (syntax->datum str)))))

(define (identifier->string stx)
  (symbol->string (syntax->datum stx)))

(define-syntax (format->identifier stx)
  (syntax-case stx ()
    ((_ stx-obj fmt arg ...)
     #'(string->identifier stx-obj (format fmt (syntax->datum arg) ...)))))

;; dot access style

(define-syntax (dot-part-impl stx)
  (syntax-case stx ()
    ((k type-name var-name func-name arg ...)
     (with-syntax ((real-func (format->identifier #'k "~a-~a" #'type-name #'func-name))
                   (obj (string->identifier #'k #'var-name)))
       #'(real-func obj arg ...)))))

(define-syntax (dot-part stx)
  (syntax-case stx ()
    ((_ name arg ...)
     (let-values (((var-name func-name) (string-split (identifier->string #'name) #\.)))
       (if (string-empty? func-name)
           #'(name arg ...)
           (with-syntax* ((var-literal-name (datum->syntax #'name var-name))
                          (func-literal-name (datum->syntax #'name func-name))
                          (var-type-name (object-type-string #'var-literal-name)))
             #'(dot-part-impl var-type-string var-literal-name func-literal-name arg ...))))))

(define-syntax (dot stx)
  (syntax-case stx ()
    ((_ (name arg ...) ...)
     #'(begin (dot-part name arg ...) ...))))

