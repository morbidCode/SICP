(define (smooth input-stream)
(stream-map 
(lambda (element next-element) 
(/ (+ element next-element) 2)) 
input-stream (stream-cdr input-stream)))

(define zero-crossings 
  (make-zero-crossings (cons-stream 0 (smooth sense-data)) 0))
