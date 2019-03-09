(define (log2-summands n)
  (cons-stream 
   (/ 1.0 n)
   (stream-map - (log2-summands (+ n 1)))))

(define log2-stream
(partial-sums (log2-summands 1)))

(define euler-transform-log2-stream (euler-transform log2-stream))

(define accelerated-sequence-log2-stream (accelerated-sequence euler-transform log2-stream))
