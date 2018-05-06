
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

(define-syntax with-values
  (syntax-rules ()
    ((_ expr proc)
     (call-with-values (lambda () expr) proc))))

(define-syntax datum
  (syntax-rules ()
    [(_ t) (syntax->datum #'t)]))

(define-syntax (with-syntax stx)
    (syntax-case stx ()
      [(_ ((p e) ...) b1 b2 ...)
       #'(syntax-case (list e ...) ()
           [(p ...) (let () b1 b2 ...)])])))

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

