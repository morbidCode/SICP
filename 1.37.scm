(define (cont-frac n d k)
(define (eval i)
(if (> i k)
0.0
(/ (n i) (+ (d i) (eval (+ i 1))))))
(eval 1))

(define (cont-frac n d k)
(define (cont-frac-iter a i)
(if (= i 0)
a
(cont-frac-iter (/ (n i) (+ (d i) a)) (- i 1))))
(cont-frac-iter 0.0 k))
