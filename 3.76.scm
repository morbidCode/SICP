(define (smooth input-stream)
(stream-map 
(lambda (element next-element) 
(/ (+ element next-element) 2)) 
input-stream (stream-cdr input-stream)))

(define smoothed-sense-data (cons-stream 0 (smooth sense-data)))
(define zero-crossings 
  (make-zero-crossings smoothed-sense-data 0))
