(define (weight i j) (+ (square i) (square j)))

(define (sum-of-squares-3-ways)
(define weights (stream-map (lambda (x) (weight (car x) (cadr x))) (weighted-pairs integers integers weight)))

(define (generate s)
(if (= (stream-car s) (stream-car (stream-cdr s)) (stream-car (stream-cdr (stream-cdr s))))
(cons-stream (stream-car s) (generate (stream-cdr s)))
(generate (stream-cdr s))))
(generate weights))
