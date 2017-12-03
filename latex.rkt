#lang racket

(define auto-parenthesize? (make-parameter #false))
(define (parenthesize s) (~a "\\left(" s "\\right)"))

(define (group s) (~a "{" s "}"))

(define (^ s1 s2) (~a s1 '^ s2))

(require (only-in "match.rkt" cat))

(define (→latex expr) (cat ↓ →latex)
  (define parenthesize′     (if (auto-parenthesize?) identity parenthesize))
  (define auto-parenthesize (if (auto-parenthesize?) parenthesize identity))
  (group (match expr
           ; Delegated:
           [`(sqr ,e) (↓ `(power ,e 2))]
           ; Compound:
           [`(power ,(↓ e1) ,(↓ e2)) (auto-parenthesize (^ e1 e2))]
           [`(+ ,(↓ e) ...) (auto-parenthesize (apply ~a #:separator "+" e))]
           ; Explicit parenthesization:
           [`(,(↓ e)) (parenthesize′ e)]
           ; Atomic:
           [n #:when (number? n) (~a n)])))

(module+ test (require rackunit)
  (parameterize ([auto-parenthesize? #true])
    (check-equal? (→latex 123) "{123}")
    (check-equal? (→latex '(power 123 456)) "{\\left({123}^{456}\\right)}")
    (check-equal? (→latex '(sqr 123)) "{{\\left({123}^{2}\\right)}}")
    (check-equal? (→latex '(sqr (power 123 456)))
                  "{{\\left({\\left({123}^{456}\\right)}^{2}\\right)}}"))
  (check-equal? (→latex '(sqr ((power ((+ 12 34 56)) 78))))
                "{{{\\left({{\\left({{12}+{34}+{56}}\\right)}^{78}}\\right)}^{2}}}"))
