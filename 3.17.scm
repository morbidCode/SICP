(define (count-pairs x)
(let ((aux (list)))

(define (in-list? lst item)
(cond ((null? lst) #f)
((eq? (car lst) item) #t)
(else (in-list? (cdr lst) item))))

(define (count x)
(if (or (not (pair? x)) (null? x) (in-list? aux x))
0
(begin (set! aux (cons x aux))
(+ (count (car x)) (count (cdr x)) 1))))
(count x)))
