(define (weight i j) (+ (cube i) (cube j)))

(define (ramanujan-numbers)
(define weights (stream-map (lambda (x) (weight (car x) (cadr x))) (weighted-pairs integers integers weight)))

(define (generate s)
(if (= (stream-car s) (stream-car (stream-cdr s)))
(cons-stream (stream-car s) (generate (stream-cdr s)))
(generate (stream-cdr s))))
(generate weights))
