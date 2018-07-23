(define (even? n) (= (remainder n 2) 0))

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (mul x y)
(if (even? y)
(double (mul x (halve y)))
(+ x (* x (- y 1)))))
