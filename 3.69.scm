(define (triples s t u)
(let ((first (stream-car s))
(stream-pairs (pairs t u)))
(cons-stream 
(cons first (stream-car stream-pairs)) 
(interleave 
(stream-map (lambda (x) 
(cons first x)) (stream-cdr stream-pairs)) 
(triples (stream-cdr s) (stream-cdr t) (stream-cdr u))))))

(define pythagorean-triples-of-positive-integers (stream-filter (lambda (x) (= (+ (square (car x)) (square (cadr x))) (square (caddr x)))) (triples integers integers integers)))
