
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


(define (construct-name ctx . args)
  (string->identifier ctx
    (apply string-append
      (map (lambda (x) (if (string? x) x (identifier->string x)))
           args))))

(define (do-def-rec-type record-name field-name-list parent-rec-type-desc)
  (with-syntax ([record-name record-name]
                [rec-type-desc (make-record-type-descriptor
                                 (syntax->datum record-name) parent-rec-type-desc #f #f #f
                                 (list->vector
                                   (map (lambda (field-name) `(immutable ,(syntax->datum field-name)))
                                        field-name-list)))]
                [make-record (construct-name record-name "make-" record-name)]
                [record-name? (construct-name record-name record-name "?")]
                [(record-field ...)
                 (map (lambda (field-name) (construct-name field-name record-name "-" field-name))
                      field-name-list)]
                [(i ...) (enumerate field-name-list)])
    #'(begin
        (define-syntax record-name (make-compile-time-value 'rec-type-desc))
        (define rec-ctor-desc (make-record-constructor-descriptor 'rec-type-desc #f #f))
        (define make-record (record-constructor rec-ctor-desc))
        (define record-name? (record-predicate 'rec-type-desc))
        (define record-field (record-accessor 'rec-type-desc i))
        ...)))

(define-syntax (def-rec-type stx)
  (syntax-case stx ()
      [(_ record-name (field-name ...))
       (for-all identifier? #'(record-name field-name ...))
       (do-def-rec-type #'record-name #'(field-name ...) #f)]
      [(_ record-name parent-name (field-name ...))
       (for-all identifier? #'(record-name parent-name field-name ...))
       (lambda (lookup)
         (let ([parent-rec-type-desc (lookup #'parent-name)])
           (unless (record-type-descriptor? parent-rec-type-desc)
             (syntax-error #'parent-name "unrecognized parent record type"))
           (do-def-rec-type #'record-name #'(field-name ...) parent-rec-type-desc)))]))

(drt prec (x y))
(drt crec prec (z))
(define r (make-crec 1 2 3))
(prec? r) <graphic> #t
(prec-x r) <graphic> 1
(crec-z r) <graphic> 3
prec <graphic> exception: invalid syntax prec




(define (dot-part-call proc args)
  (apply proc args))

(define-syntax (dot-part-impl stx)
  (syntax-case stx ()
    ((k type-name var-name func-name arg ...)
     (with-syntax ((real-func (format->identifier #'k "~a-~a" #'type-name #'func-name))
                   (obj (string->identifier #'k #'var-name)))
       (printf "dot-part-impl ~s ~s\n" #'real-func #'obj)
       #'(dot-part-call real-func (list obj arg ...))))))

(define-syntax










