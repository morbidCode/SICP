(define (cycle? lst)
(let ((orig lst))
(define (eqs? lst)
(cond ((null? lst) #f)
((eq? orig lst) #t)
(else (eqs? (cdr lst)))))

(eqs? (cdr lst))))
