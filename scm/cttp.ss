(library (lnss cttp) ;; compile time type
  (export define-type-property
          get-type-property
          assign-type-property
          dot dot-values dot-def dot-let dot-let*
          test-cttp)
  (import (lnss core))

  ;; compile time type property

  (define type)

  (define-syntax (define-type-property stx)
    (syntax-case stx ()
      ((_ var value)
       (identifier? var)
       #'(define-property var type value))))

  (define-syntax (get-type-property stx)
    (syntax-case stx ()
      ((_ var)
       (identifier? var)
       (lambda (lookup)
         (let ((obj-type (lookup #'var #'type)))
           (if (eq? #f obj-type) (syntax-error #'var "unknown type of object: " (identifier->string #'var)))
           #`'#,(datum->syntax #'var obj-type))))))

  (define-syntax (assign-type-property stx)
    (syntax-case stx ()
      ((_ var obj)
       (and (identifier? var) (identifier? obj))
       (lambda (lookup)
         (let ((obj-type (lookup #'obj #'type)))
           (if (eq? #f obj-type) (syntax-error #'obj "unknown type of object: " (identifier->string #'obj)))
           #`(begin
               (define var obj)
               (define-type-property var '#,(datum->syntax obj-type))))))))

  ;; dot access style

  (define-syntax (dot-part-impl stx)
    (syntax-case stx ()
      ((_ ctx type-name obj-name func-name arg ...)
       (with-syntax ((real-func (format->identifier #'ctx "~a-~a" #'type-name #'func-name))
                     (obj (string->identifier #'ctx #'obj-name)))
         #'(real-func obj arg ...)))))

  (define-syntax (dot-part stx)
    (syntax-case stx ()
      ((_ name arg ...)
       (let-values (((obj-name func-name) (string-split (identifier->string #'name) #\.)))
         (if (or (string-empty? obj-name) (string-empty? func-name))
             #'(name arg ...)
             (lambda (lookup)
               (let ((obj-type (lookup (string->identifier #'name obj-name) #'type)))
                 (if (eq? #f obj-type) (syntax-error #'name "unknown type of object: " obj-name))
                 #`(dot-part-impl
                     name
                     #,(datum->syntax #'name (symbol->string obj-type))
                     #,(datum->syntax #'name obj-name)
                     #,(datum->syntax #'name func-name)
                     arg ...))))))))

  (define-syntax (dot stx)
    (syntax-case stx ()
      ((_ (name arg ...) ...)
         #'(begin (dot-part name arg ...) ...))))

  (define-syntax (dot-values stx)
    (syntax-case stx ()
      ((_ (name arg ...) ...)
       #'(values (dot-part name arg ...) ...))))

  (define-syntax (dot-def stx)
    (syntax-case stx ()
      ((_ ((var (name arg ...)) ...))
       #'(begin
           (define var (dot-part name arg ...))
           ...))))

  (define-syntax (dot-let stx)
    (syntax-case stx ()
      ((_ ((var (name arg ...)) ...) body body2 ...)
       #'(let ((var (dot-part name arg ...)) ...) body body2 ...))))

  (define-syntax (dot-let* stx) 
    (syntax-case stx ()
      ((_ ((var (name arg ...)) ...) body body2 ...)
       #'(let* ((var (dot-part name arg ...)) ...) body body2 ...))))

  ;; symbol table with compile time type

  (define-syntax (symbol-table-set! stx)
    (syntax-case stx ()
      ((_ t symbol value) #'(symbol-hashtable-set! t symbol value))))

  (define-syntax (symbol-table-del! stx)
    (syntax-case stx ()
      ((_ t symbol) #'(symbol-hashtable-delete! t symbol))))

  (define-syntax (symbol-table-has? stx)
    (syntax-case stx ()
      ((_ t symbol) #'(symbol-hashtable-contains? t symbol))))

  (define (symbol-table-ref t symbol)
      (symbol-hashtable-ref t symbol (void)))

  (define (symbol-table-cell t symbol)
      (symbol-hashtable-cell t symbol (void)))

  (define-syntax (symbol-table stx)
    (syntax-case stx ()
      ((_ var size (symbol value) ...)
       #'(begin
           (define var (make-hashtable symbol-hash eq? size))
           (define-type-property var 'symbol-table)
           (symbol-hashtable-set! var symbol value)
           ...))))

  (define (test-cttp)
    (symbol-table st 32 ('mode "direct"))
    (assert-eq 'symbol-table (get-type-property st))
    (assert-equal "direct" (dot (st.ref 'mode)))))

