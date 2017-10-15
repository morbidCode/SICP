(define (predmap pred lst)
    (if (null? lst) '()
        (let ((carst (car lst))
            (next (predmap pred (cdr lst))))
            (if (pred carst)
                (cons carst next)
                next))))

(define (same-parity . lst)
    (if (null? lst) '()
        (cons
            (car lst)
            (predmap (if (even? (car lst)) even? (lambda (x) (not (even? x))))
            (cdr lst)))))
