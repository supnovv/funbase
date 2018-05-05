
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

