(define (inc x) (+ x 1))

(define (filtered-accumulate combiner null-value pred term a next b)
(if (> a b) null-value
(combiner (if (pred a) (term a) null-value) (filtered-accumulate combiner null-value pred term (next a) next b))))

(define (prime-sum-square a b)
(filtered-accumulate + 0 prime? square a inc b))

(define (relative-prime-product n)
(define (relative-prime? i)
(= (gcd i n) 1))
(define (identity x) x)
(filtered-accumulate * 1 relative-prime? identity 1 inc (- n 1)))
