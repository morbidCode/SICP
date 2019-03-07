(define (div-series s1 s2)
(let ((s2-c (stream-car s2)))
(define (generate) (cons-stream (/ (stream-car s1) s2-c) (stream-map (lambda (x) (/ x s2-c)) (add-streams (stream-cdr s1) (mul-series (stream-map - (stream-cdr s2)) (generate))))))
(if (= s2-c 0)
(error "Denominator has zero constant term: DIV-SERIES" s2)
(generate))))

(define tangent-series (div-series sine-series cosine-series))
