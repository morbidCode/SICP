(define (rand s)
(define random-numbers
  (cons-stream random-init
               (stream-map 
(lambda (number message)
(cond ((and (pair? message) (eq? (car message) 'reset) (number? (cdr message))) (cdr message))
((eq? message 'update) (rand-update number))
(else (error "Unknown symbol: RAND" message))))
                           random-numbers s)))
random-numbers)
