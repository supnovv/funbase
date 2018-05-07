
;; ## assert helpers
;;
;; (assert-enable-print! enable) -> void
;; (assert-tr expr) -> void
;; (assert-nt expr) -> void
;; (assert-eq expr1 expr2) -> void
;; (assert-nq expr1 expr2) -> void

(define assert-pass-print? #t)

(define (assert-enable-print! enable)
  (set! assert-pass-print? (if enable #t #f)))

(define-syntax (assert-pass-print stx)
  (syntax-case stx ()
    ((_ ctx expr) (not assert-pass-print?) #'(void))
    ((_ ctx expr) #'(printf "[A] PASS ~s\n" expr))
    ((_ ctx msg expr) (not assert-pass-print?) #'(void))
    ((_ ctx msg expr) #'(printf "[A] PASS ~s: ~s\n" msg expr))))

(define-syntax (assert-fail-print stx)
  (syntax-case stx ()
    ((_ ctx expr) #'(printf "[A] FAIL ~s\n" expr))
    ((_ ctx msg expr) #'(printf "[A] FAIL ~s: ~s\n" msg expr))
    ((_ ctx expr a b) #'(printf "[A] FAIL ~s ~s ~s\n" expr a b))
    ((_ ctx msg expr a b) #'(printf "[A] FAIL ~s: ~s ~s ~s\n" msg expr a b))))

(define-syntax (assert-fail stx)
  (syntax-case stx ()
    ((name msg)
     #'(assert-fail-print name msg))))

(define-syntax (assert-tr stx)
  (syntax-case stx ()
    ((name expr)
     #'(cond
         (expr (assert-pass-print name '(name expr)))
         (else (assert-fail-print name '(name expr)))))))

(define-syntax (assert-nt stx)
  (syntax-case stx ()
    ((name expr)
     #'(cond
         (expr (assert-fail-print name '(name expr)))
         (else (assert-pass-print name '(name expr)))))))

(define-syntax (assert-eq stx)
  (syntax-case stx ()
    ((name expr-a expr-b)
     #'(let ((x expr-a) (y expr-b))
         (cond
           ((eq? x y) (assert-pass-print name '(name expr-a expr-b)))
           (else (assert-fail-print name '(name expr-a expr-b) x y)))))))

(define-syntax (assert-nq stx)
  (syntax-case stx ()
    ((name expr-a expr-b)
     #'(let ((x expr-a) (y expr-b))
         (cond
           ((eq? x y) (assert-fail-print name '(name expr-a expr-b)))
           (else (assert-pass-print name '(name expr-a expr-b))))))))

;; ## macro helpers
;;
;; (string->identifier ctx str) -> identifer-syntax
;; (format->identifier ctx fmt arg ...) -> identifier-syntax
;; (identifier->string stx) -> string
;; (eval-syntax stx)

(define (string->identifier ctx str)
  (if (string? str)
      (datum->syntax ctx (string->symbol str))
      (datum->syntax ctx (string->symbol (syntax->datum str)))))

(define-syntax (format->identifier stx)
  (syntax-case stx ()
    ((_ ctx fmt arg ...)
     #'(string->identifier ctx (format fmt (syntax->datum arg) ...)))))

(define (identifier->string stx)
  (symbol->string (syntax->datum stx)))

(define-syntax (eval-syntax stx)
  (syntax-case stx ()
    ((_ syntax-expr) #'(eval (syntax->datum syntax-expr)))))

;; ## compile time property
;;
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
;;
;; (attach-prop var prop value) -> void
;; (attach-type var value) -> void
;; (attach-string-type var) -> void
;; (attach-symbol-table-type var) -> void
;; (obtain-prop var prop) -> property-value
;; (obtain-prop-from identifier/literal-string prop) -> property-value
;; (obtain-type identifier/literal-string) -> type-property-value
;; (obtain-type->string var) -> type-property-value-as-string

(define-syntax (attach-prop stx)
  (syntax-case stx ()
    ((_ var prop value)
     (and (identifier? #'var) (identifier? #'prop))
     #'(define-property var prop value))))

(define-syntax (obtain-prop stx)
  (lambda (lookup)
    (syntax-case stx ()
      ((_ var prop)
       #`'#,(datum->syntax #'var (lookup #'var #'prop))))))

(define-syntax (obtain-prop-from stx)
  (syntax-case stx ()
    ((name identifier/literal-string prop)
     (identifier? #'identifier/literal-string)
     #'(obtain-prop identifier/literal-string prop))
    ((name identifier/literal-string prop)
     (with-syntax ((identifier (string->identifier #'name #'identifier/literal-string)))
       #'(obtain-prop identifier prop)))))

(define type)

(define-syntax (attach-type stx)
  (syntax-case stx ()
    ((_ var value) #'(attach-prop var type value))))

(define-syntax (obtain-type stx)
  (syntax-case stx ()
    ((_ identifier/literal-string)
     #'(obtain-prop-from identifier/literal-string type))))

(define-syntax (attach-string-type stx)
  (syntax-case stx ()
    ((_ var) #'(attach-type var 'string))))

(define-syntax (attach-symbol-table-type stx)
  (syntax-case stx ()
    ((_ var) #'(attach-type var 'symbol-table))))

(define-syntax (obtain-type->string stx)
  (syntax-case stx ()
    ((_ var) #'(symbol->string (obtain-type var)))))

(define-syntax (test-prop stx)
  (syntax-case stx ()
    ((_)
     #'(let ((s "hello"))
         (attach-string-type s)
         (assert-eq 'string (obtain-type s))
         (assert-eq 'string (obtain-type "s"))
         (printf "(obtain-type s) ~s\n" (obtain-type s))
         (printf "(obtain-type \"s\") ~s\n" (obtain-type "s"))
         (printf "(obtain-type->string s) ~s\n" (obtain-type->string s))
         (printf "(obtain-type->string \"s\") ~s\n" (obtain-type->string "s"))))))

;; ## string operations
;;
;; (string-empty? str) -> boolean
;; (string-split str ch) -> string-part-one string-part-two

(define (string-empty? str)
  (eq? 0 (string-length str)))

(define (string-split str ch) ;; split a string into two parts, ch must be a character
  (let ((len (string-length str)))
    (let find ((n 0))
      (if (< n len)
          (if (eq? ch (string-ref str n))
              (values (substring str 0 n) (substring str (+ n 1) len))
              (find (+ n 1)))
          (values str "")))))

;; ## dot access style
;;
;; (dot (obj.func arg ...) ...)
;; (dot-values (obj.func arg ...) ...)
;; (dot-def ((var (obj.func arg ...)) ...))
;; (dot-let ((var (obj.func arg ...)) ...) body body2 ...)
;; (dot-let* ((var (obj.func arg ...)) ...) body body2 ...)

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
       (if (or (string-empty? var-name) (string-empty? func-name))
           #'(name arg ...)
           (let ((type-name (eval-syntax #`(obtain-type->string #,(datum->syntax #'name var-name)))))
             #`(dot-part-impl
                 #,(datum->syntax #'name type-name)
                 #,(datum->syntax #'name var-name)
                 #,(datum->syntax #'name func-name)
                 arg ...)))))))

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

(define s "hello")
(attach-string-type s)
(printf "(obtain-type s) ~s\n" (obtain-type s))
(printf "(dot (s.ref 0)) ~s\n" (dot (s.ref 0)))
(dot-def ((char-at (s.ref 1)) (str-len (s.length))))
(printf "(dot-def ((char-at (s.ref 0)) (str-len (s.length)))) ~s ~s\n" char-at str-len)
(printf "(dot (string-ref s 0)) ~s\n" (dot (string-ref s 0)))

;; 匹配变量指向的是表达式中对应位置的数据，如数字、字符串、列表等，包裹成的语法对象
;; 匹配变量的#'操作相当于获取匹配变量的值，即对应的语法对象，但是普通标识符和数据的#'操作会生成标识符语法对象和对应的数据语法对象
;; 语法对象需要进行一次 syntax->datum 的操作才能得到原本的数据本身
;; #'操作会获取其中匹配变量的值，还会将普通标识符和数据转换成语法对象
;; #`操作与#'类似，但是还会对其中的#,表达式求值
;; 由于 with-syntax 可以手动给匹配变量赋值，匹配变量可能被置成实际的数据而不是包裹后的语法对象
;; 此时对匹配变量的#'操作，也仅仅是获取匹配变量的值，这不是问题，因为普通数据的 syntax->datum 操作简单返回数据本身
;; 如果一个标识符的值是一个编译时常量，datum->syntax 可以将这个标识符转换成常量语法对象
;; 即使一个标识符的值是一个运行时数据，例如是字符串、数字等，通过调用 eval 和 datum->syntax 也可以将这个标识符转换成常量语法对象

(define-syntax (syntax-test stx)
  (syntax-case stx ()
    ((k pattern-var) (identifier? #'pattern-var)
     (let ((plain-var 0))
       (printf "#`(proc plain-var pattern-var #,(datum->syntax #'* string)) ~s\n" #`(proc plain-var pattern-var #,(datum->syntax #'* "string")))
       (printf "#`(proc plain-var pattern-var #,string) ~s\n" #`(proc plain-var pattern-var #,"string"))
       (with-syntax ((with-pattern-var "string")
                     (with-pattern-var-string (datum->syntax #'k "string")))
         (printf "#'(proc with-pattern-var with-pattern-var-string) ~s \n" #'(proc with-pattern-var with-pattern-var-string))
         #'(printf "syntax-test ~s\n" 'pattern-var))))
    ((k pattern-var)
     (begin
       (printf "syntax-test ~s\n" #'pattern-var)
       #'(void)))))

(syntax-test var)
(syntax-test "string")
(syntax-test 'symbol)
(syntax-test 1024)
(syntax-test #\A)
(test-prop)

