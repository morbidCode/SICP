(define (cons a b)
    (* (expt 2 a) (expt 3 b)))

(define (repeat-divide x y)
    (if (> (remainder x y) 0)
        0
    (+ 1 (repeat-divide (/ x y) y))))

(define (car p) (repeat-divide p 2))
(define (cdr p) (repeat-divide p 3))
