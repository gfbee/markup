#lang racket


(define auto-parenthesize? (make-parameter #false))
(define (group s) (if (auto-parenthesize?) (parenthesize s) (~a "{" s "}")))
(define parenthesize (curry format "{\\left({~a}\\right)}"))

(require (only-in "match.rkt" cat))

(define (→latex expr) (cat ↓ →latex)
  (match expr
    [`(sqr ,e) (↓ `(power ,e 2))]
    [`(power ,(↓ e1) ,(↓ e2)) (~a (group e1) '^ (group e2))]
    [`(+ ,(↓ e1) ,(↓ e2)) (~a (group e1) '+ (group e2))]
    [`(,(↓ e)) (parenthesize e)]
    [n #:when (number? n) (~a n)]))

(module+ test (require rackunit)
  (parameterize ([auto-parenthesize? #true])
    (check-equal? (→latex 123) "123")
    (check-equal? (→latex '(power 123 456)) "{\\left({123}\\right)}^{\\left({456}\\right)}")
    (check-equal? (→latex '(sqr 123)) "{\\left({123}\\right)}^{\\left({2}\\right)}")
    (check-equal? (→latex '(sqr (power 123 456)))
                  (~a "{\\left({{\\left({123}\\right)}^{\\left({456}\\right)}}\\right)}"
                      "^{\\left({2}\\right)}")))
  (check-equal? (→latex '(sqr ((power ((+ 123 678)) 456))))
                "{{\\left({{{\\left({{123}+{678}}\\right)}}^{456}}\\right)}}^{2}"))
