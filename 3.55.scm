(define (partial-sums s)
(if (stream-null? s)
the-empty-stream
  (cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sums s)))))
