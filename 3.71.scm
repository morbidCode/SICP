(define (weight i j) (+ (cube i) (cube j)))

(define (ramanujan-numbers)
(define weights (stream-map (lambda (x) (weight (car x) (cadr x))) (weighted-pairs integers integers weight)))
(define (generate s t)
(if (= (stream-car s) (stream-car t))
(cons-stream (stream-car s) (generate (stream-cdr s) (stream-cdr t)))
(generate (stream-cdr s) (stream-cdr t))))
(generate weights (stream-cdr weights)))
