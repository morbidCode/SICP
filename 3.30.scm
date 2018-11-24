(define (ripple-carry-adder a b sum c)
(if (null? sum)
'ok
(let ((c-in (make-wire)))
(ripple-carry-adder (cdr a) (cdr b) (cdr sum) c-in)
(full-adder (car a) (car b) c-in (car sum) c))))
