(define (cycle? lst)
(define (cycle-iter cur-ptr next-ptr)
(cond ((or (null? (cdr next-ptr)) (null? (cddr next-ptr))) #f)
((eq? cur-ptr next-ptr) #t)
(else (cycle-iter (cdr cur-ptr) (cddr next-ptr)))))
(if (or (null? lst) (null? (cdr lst)))
#f
(cycle-iter lst (cdr lst))))
