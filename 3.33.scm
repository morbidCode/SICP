(define (averager a b c)
(let ((d (make-connector))
(m1 (make-connector))
(m2 (make-connector)))
(multiplier m1 d a)
(multiplier m2 d b)
(adder m1 m2 c)
(constant 2 d)
'ok))
