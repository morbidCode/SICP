	(define (mul-streams s1 s2) 
  (stream-map * s1 s2))

(define (integrate-series s)
(mul-streams s (stream-map (lambda (x) (/ 1 x)) integers)))

(define cosine-series (cons-stream 1 (stream-map - (integrate-series sine-series))))

(define sine-series (cons-stream 0 (integrate-series cosine-series)))
