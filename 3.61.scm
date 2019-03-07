(define (invert-unit-series s) 
(cons-stream 
1 
(mul-series 
(stream-map - (stream-cdr s)) 
(invert-unit-series s))))
