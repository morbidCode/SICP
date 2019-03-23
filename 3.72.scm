(define (weight i j) (+ (square i) (square j)))

(define (sum-of-squares-3-ways)
(define weights (stream-map (lambda (x) (weight (car x) (cadr x))) (weighted-pairs integers integers weight)))
(define (generate s t)
(if (= (stream-car s) (stream-car (stream-cdr s)) (stream-car t))
(cons-stream (stream-car s) (generate (stream-cdr s) (stream-cdr t)))
(generate (stream-cdr s) (stream-cdr t))))
(generate weights (stream-cdr (stream-cdr weights))))
