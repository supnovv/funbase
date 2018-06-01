

;; ## assert helpers
;;
;; (assert-enable-print! enable) -> void
;; (assert-fail msg) -> void
;; (assert-tr expr) -> void
;; (assert-nt expr) -> void
;; (assert-eq expr1 expr2) -> void
;; (assert-nq expr1 expr2) -> void


;; ## macro helpers
;;
;; (string->identifier ctx str) -> identifer-syntax
;; (format->identifier ctx fmt arg ...) -> identifier-syntax
;; (identifier->string stx) -> string
;; (eval-syntax stx)


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

(define (test-symbol-table)
  (symbol-table t 32 ('mode "direct") ('type "strength"))
  (printf "type ~s\n" (obtain-type->string t))
  (printf "mode ~s type ~s\n" (apply symbol-table-cell (list t 'type)) (dot (t.ref 'mode))))
  #|(dot-let* ((has-mode (t.has? 'mode))
             (mode (t.ref 'mode))
             (pair (t.cell 'type))
             (type (begin (t.set! 'type "wisdom") (t.ref 'type))))
    (printf "has-mode ~s mode ~s old-type ~s new-type ~s\n"
            has-mode mode (cdr pair) type))
  (let ((new-mode (dot (t.del! 'mode) (t.ref 'mode))))
    (printf "new-mode is (void) ~s after delete\n" (eq? new-node (void)))))|#

(test-symbol-table)

