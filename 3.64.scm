(define (stream-limit s tolerance)
(if (stream-null? (stream-cdr s))
(error "No result with acceptable tolerance found." tolerance)
(let ((second-element (stream-car (stream-cdr s))))
(if (< (abs (- second-element (stream-car s))) tolerance)
second-element
(stream-limit (stream-cdr s) tolerance)))))
