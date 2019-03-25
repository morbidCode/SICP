(define (smooth input-stream)
(stream-map 
(lambda (input next-input) 
(/ (+ input next-input) 2)) 
input-stream (stream-cdr input-stream)))

(define smoothed-sense-data (smooth sense-data))
(define zero-crossings 
  (make-zero-crossings smoothed-sense-data 0))
