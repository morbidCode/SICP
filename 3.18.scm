(define (cycle? lst)
(let ((aux (list)))

(define (in-list? lst item)
(cond ((null? lst) #f)
((eq? (car lst) item) #t)
(else (in-list? (cdr lst) item))))

(define (compare lst)
(cond ((null? lst) #f)
((in-list? aux lst) #t)
(else (set! aux (cons lst aux))
(compare (cdr lst)))))
(compare lst)))
