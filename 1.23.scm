(define (next n) (if (= n 2) 3 (+ n 2)))

(define (divides? a b) (= (remainder b a) 0))

(define (find-divisor n d)
(cond ((> (square d) n) n)
((divides? d n) d)
(else (find-divisor n (next d)))))

(define (smallest-divisor n) (find-divisor n 2))
(define (prime? n) (= (smallest-divisor n) n))
