(define (or-gate a b out)
(let ((c (make-wire))
(d (make-wire))
(e (make-wire)))
(inverter a c)
(inverter b d)
(and-gate c d e)
(inverter e out)))