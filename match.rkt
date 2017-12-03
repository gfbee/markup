#lang racket #| Match |#

(provide cat)

#| Catamorphism : auto-recursing pattern.

 (cat pat:id f:expr)

   Define ‘(pat . <pattern>)’ as a new match pattern that first calls ‘f’
    and then matches the result against the remaining ‘<pattern>’.

 In that form, it's not limited to recursive calls. |#

(require (for-syntax syntax/parse))
(define-syntax cat
  (syntax-parser [(_ pat:id f:expr) #'(define-match-expander pat
                                        (syntax-parser [(_ . pattern) #'(app f . pattern)])
                                        (syntax-parser [(_ . sub-forms) #'(f . sub-forms)]))]))

(module+ test (require rackunit)
  (define (f e) (cat ↓ f) (cat S sqr) (cat neg -)
    (match e
      [`(sqr ,(↓ (S e))) e]
      [`(+ ,(↓ e) ...) (apply + e)]
      [`(- ,(↓ e)) (neg e)]
      [_ e]))
  (check-equal? (f '(- (sqr (+ 1 2 3)))) (- (sqr (+ 1 2 3)))))
