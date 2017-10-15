#web

(define (inc x) (+ x 1))

1.
(define (product term a next b)
(if (> a b)
1
(* (term a) (product term (next a) next b))))

(define (factorial n)
(define (identity x) x)
(product identity 1 inc n))

(define (pi-product n)
(define (numer-term x) (* (* 2.0 x) (+ (* 2.0 x) 2.0)))
(define (denom-term x) (square (+ (* 2.0 x) 1.0)))
;; (define (term x) (/ (numer-term x) (denom-term x)))

(define (term n)
(if (even? n)
(/ (+ k 2) (+ k 1))
(/ (+ k 1) (+ k 2))))
(product term 1 inc n))

2.
(define (product term a next b)
(define (iter a result)
(if (> a b)
result
(iter (next a) (* (term a) result))))
(iter a 1))
