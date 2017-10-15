(define (adjoin-set lst x)
(cond ((or (null? lst) (> (car lst) x)) (cons x lst))
((= (car lst) x) lst)
(else (cons (car lst) (adjoin-set (cdr lst) x)))))
