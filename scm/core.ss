(library (lnss core)
         (export string-empty? string-split
                 (import (chezscheme)))
         (import (chezscheme))

;; string

(define (string-empty? str)
  (eq? 0 (string-length str)))

(define (string-split s ch) ;; split a string into two parts, ch must be a character
  (let ((len (string-length s)))
    (let find ((n 0))
      (if (< n len)
          (if (eq? ch (string-ref s n))
              (values (substring s 0 n) (substring s (+ n 1) len))
              (find (+ n 1)))
          (values s "")))))

;; syntax

(define (string->id ctx s)
  (if (string? s)
      (datum->syntax ctx (string->symbol s))
      (datum->syntax ctx (string->symbol (syntax->datum s)))))

(define-syntax (format->id stx)
  (syntax-case stx ()
    ((_ ctx fmt arg ...)
     #'(string->id ctx (format fmt (syntax->datum arg) ...)))))

(define (id->string stx)
  (symbol->string (syntax->datum stx)))

(define (eval-syntax stx)
  (eval (syntax->datum stx)))

;; annotations
;;
;; source-file-descriptor
;; => file path (string)
;;    file checksum (number)
;;
;; source-object
;; => source-file-descriptor (sfd)
;;    beginning file position (bfp, number)
;;    ending file position (efp, number)
;;    beginning line (optional, number)
;;    begining column (optional, number)
;;
;; annotation
;; => expression
;;    source object
;;    stripped version of the expression
;;    usage options (debug or profile)

(define (syntax-file stx)
  (let ((annotation (syntax->annotation stx)))
    (if (eq? annotation #f) "" 
        (source-file-descriptor-path (source-object-sfd (annotation-source annotation))))))

(define (syntax-fsum stx)
  (let ((annotation (syntax->annotation stx)))
    (if (eq? annotation #f) 0
        (source-file-description-checksum (source-object-sfd (annotation-source annotation))))))

(define (syntax-bpos stx)
  (let ((annotation (syntax->annotation stx)))
    (if (eq? annotation #f) 0
        (source-object-bfp (annotation-source annotation)))))

(define (syntax-epos stx)
  (let ((annotation (syntax->annotation stx)))
    (if (eq? annotation #f) 0
        (source-object-efp (annotation-source annotation)))))

(define (syntax-line stx)
  (let ((annotation (syntax->annotation stx)))
    (if (eq? annotation #f) 0
        (let ((ln (source-object-line (annotation-source annotation))))
          (if (eq? ln #f) 0 ln)))))

(define (syntax-column stx)
  (let ((annotation (syntax->annotation stx)))
    (if (eq? annotation #f) 0
        (let ((col (source-object-column (annotation-source annotation))))
          (if (eq? col #f) 0 col)))))

;; assert

(define assert-pass-print? #t)

(define (assert-enable-print! enable)
  (set! assert-pass-print? (if enable #t #f)))

(define-syntax (assert-print stx)
  (syntax-case stx ()
    ((_ ctx pass fmt ...)
     (let ((pass? (syntax->datum #'pass)))
       (if (and pass? (not assert-pass-print?)) #'(void)
           (with-syntax ((fmts (datum->syntax #'ctx (string-append "[A] " (if pass? "PASS" "FAIL") " ~a (~a) " (syntax->datum #'fmt))))
                         (file (datum->syntax #'ctx (syntax-file #'ctx)))
                         (line (datum->syntax #'ctx (syntax-line #'ctx))))
             #'(printf fmts file line ...)))))))

(define-syntax (assert-dr stx)
  (syntax-case stx ()
    ((name msg) #'(assert-print name #f "~s" msg))))

(define-syntax (assert-tr stx)
  (syntax-case stx ()
    ((name expr)
     #'(if expr
           (assert-print name #t "~s" '(name expr))
           (assert-print name #f "~s" '(name expr))))))

(define-syntax (assert-nt stx)
  (syntax-case stx ()
    ((name expr)
     #'(if expr
           (assert-print name #f "~s" '(name expr))
           (assert-print name #t "~s" '(name expr))))))

(define-syntax (assert-eq stx)
  (syntax-case stx ()
    ((name expr1 expr2)
     #'(if (eq? expr1 expr2)
           (assert-print name #t "~s" '(name expr1 expr2))
           (assert-print name #f "~s ~s ~s" '(name expr1 expr2) expr1 expr2)))))

(define-syntax (assert-nq stx)
  (syntax-case stx ()
    ((name expr1 expr2)
     #'(if (eq? expr1 expr2)
           (assert-print name #f "~s" '(name expr1 expr2))
           (assert-print name #t "~s" '(name expr1 expr2))))))

(define-syntax (assert-eqv stx)
  (syntax-case stx ()
    ((name expr1 expr2)
     #'(if (eqv? expr1 expr2)
           (assert-print name #t "~s" '(name expr1 expr2))
           (assert-print name #f "~s ~s ~s" '(name expr1 expr2) expr1 expr2)))))

(define-syntax (assert-nqv stx)
  (syntax-case stx ()
    ((name expr1 expr2)
     #'(if (eqv? expr1 expr2)
           (assert-print name #f "~s" '(name expr1 expr2))
           (assert-print name #t "~s" '(name expr1 expr2))))))

(define-syntax (assert-equal stx)
  (syntax-case stx ()
    ((name expr1 expr2)
     #'(if (equal? expr1 expr2)
           (assert-print name #t "~s" '(name expr1 expr2))
           (assert-print name #f "~s ~s ~s" '(name expr1 expr2) expr1 expr2)))))

(define-syntax (assert-nqual stx)
  (syntax-case stx ()
    ((name expr1 expr2)
     #'(if (equal? expr1 expr2)
           (assert-print name #f "~s" '(name expr1 expr2))
           (assert-print name #t "~s" '(name expr1 expr2))))))

;; compile time property
;;
;; define-property <name> <prop> <value>)
;; Attaches a property to an existing identifier binding without disturbing
;; the existing meaning of the identifier in the scope of that binding. It
;; is typically used by one macro to record information about a binding for
;; use by another macro. Both <name> and <prop> must be identifiers.

(define (lookup-prop lookup name prop)
  (let ((value (lookup name prop)))
    (if (eq? value #f)
        (syntax-error name "unknown cpltime property '" (id->string prop) "' for identifier '" (id->string name))
        value)))

(define-syntax (define-prop stx)
  (syntax-case stx ()
    ((_ name prop value) #'(define-property name prop value))))

(define-syntax (obtain-prop stx)
  (syntax-case stx ()
    ((_ name prop)
     (and (identifier? #'name) (identifier? #'prop))
     (lambda (lookup)
       (let ((value (lookup-prop lookup #'name #'prop)))
         #`'#,(datum->syntax #'name value))))))

(define-syntax (assign-prop stx)
  (syntax-case stx ()
    ((_ name from prop)
     (and (identifier? #'name) (identifier? #'from) (identifier? #'prop))
     (let ((value (obtain-prop #'name #'prop)))
       #`(begin
           (define name from)
           (define-prop name prop '#,(datum->syntax #'name value)))))))

(define type)

(define-syntax (define-type-prop stx)
  (syntax-case stx ()
    ((_ name value) #'(define-prop name type value))))

(define-syntax (obtain-type-prop stx)
  (syntax-case stx ()
    ((_ name) #'(obtain-prop name type))))

(define-syntax (assign-type-prop stx)
  (syntax-case stx ()
    ((_ name from) #'(assign-prop name from type))))



) ;; library end

