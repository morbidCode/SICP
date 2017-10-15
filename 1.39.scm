(define (cont-frac n d k)
(define (eval i)
(if (> i k)
0.0
(/ (n i) (+ (d i) (eval (+ i 1))))))
(eval 1))

(define (tan-cf x k)
(cont-frac (lambda (i) (if (= i 1) x (- (square x)))) (lambda (i) (+ i (- i 1))) k))
