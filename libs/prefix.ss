
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

(define-syntax (get-object-type stx)
  (syntax-case stx ()
    ((_ var) #'(get-property var type))))

(define-syntax (get-type-string stx)
  (syntax-case stx ()
    ((_ var) #'(symbol->string (get-type var)))))

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

(define (string-empty? s)
  (eq? 0 (string-length s)))

(define (string-split s c)   ;; split a string into two parts, c must be a character
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

(define (symbol-type? stx)
  (let ((d (syntax->datum stx)))
    (if (and (list? d) (not (null? d)))
        (let ((1st (car d))
              (2nd (if (null? (cdr d)) (void) (cadr d))))
          (and (or (eq? 1st 'quote) (eq? 1st 'quasiquote))
               (not (eq? 2nd (void)))
               ((identifier? (datum->syntax #'1st 2nd)))))
        #f)))

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


(define (string->identifier ctx str)
  (let ((obj (inspect/object (syntax->datum str))))
    (printf "\nobject ~s ~s: ~s ~s\n" obj (obj 'type) (car obj) (cdr obj))
    (if (symbol? obj)
        (datum->syntax ctx obj)
        (datum->syntax ctx (string->symbol obj)))))

(define-syntax (format->syntax stx)
  (syntax-case stx ()
    ((_ stx-obj fmt arg ...)
     #'(string->syntax stx-obj (format fmt arg ...)))))

(define (split-id stx sep)
  (symbol->string (syntax->datum stx)))

;; constructures attach compile-time property

(define-syntax (attach-string-type stx)
  (syntax-case stx ()
    ((_ var)
     #'(define-property var type 'string))))

;; dot access style

(define-syntax (dot stx)
  (syntax-case stx ()
    ((_ (name arg ...) (name-2 arg-2 ...) ...)
     (let-values (((var-name func-name) (split (string-id #'name) #\.)))
       (if (string-empty? func-name)
           stx
           (with-syntax* ((var (string->syntax #'name var-name))
                          (real-func (format->syntax #'name "~a-~a" (get-type-string var) func-name))))
             #'(begin
                 (real-func var arg ...)

(define-syntax (dot-part stx)
   (syntax-case stx ()
     ((_ name arg ...)
      (let-values (((var-name func-name) (string-split (string-id #'name) #\.)))
        (printf "~a ~a\n" var-name func-name)
        (if (string-empty? func-name)
            #'(name arg ...)
            (let ((var (string->syntax #'name var-name)))
              (printf "string s ~a\n" s)
              (printf "variable ~s\n" var)
              (printf "s type ~s\n" (get-type-string s))
              (printf "var type ~s\n" (get-type-string var))
              (with-syntax ((real-func (format->syntax #'name "~a-~a" (get-type-string var) func-name)))
                (printf "~a ~a\n" var #'real-func)
                #'(real-func var arg ...))))))))


(define-syntax (string-type stx)
  (syntax-case stx ()
    ((name str) (string? (syntax->datum #'str))
     (with-syntax ((var (string->syntax #'name (syntax->datum #'str))))
       (printf "printf outside ~a\n" #'var)
       #'(printf "printf inside ~a\n" var)))))

(define-syntax (string-syntax? stx)
  (

(define-syntax (object-type stx)
  (syntax-case stx ()
    ((name obj)
     (identifier? #'obj)
     #'(get-object-type obj))
    ((name obj)
     (with-syntax ((id (string->identifier #'name #'obj)))
       #'(get-object-type id)))))

(define-syntax (object-type stx)
  (syntax-case stx ()
    ((name obj)
     (identifier? #'obj)
     #'(display obj))
    ((name obj)
     (write #'obj)
     (with-syntax ((id (string->identifier #'name #'obj)))
       #'(display id)))))


(define-syntax (dot stx)
  (syntax-case stx ()
    ((_ (name arg ...) ...)
     #`(begin #,(dot-part name arg ...) ...))))


(define-syntax with-values
  (syntax-rules ()
    ((_ expr proc)
     (call-with-values (lambda () expr) proc))))



