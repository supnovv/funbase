
(define (string-syntax? stx)
  (string? (syntax->datum stx)))

(define-syntax (is-string-syntax? stx)
  (syntax-case stx ()
    ((_ var)
     (let ()
       (printf "string-syntax? ~s: ~s\n" (string-syntax? #'var) #'var)
       #'(void)))))

(define (test str)
  (is-string-syntax? str))

(define s "hello")
(test s)
(test "abc")

