(define (cubic a b c)
(lambda (x) 
(+ (expt x 3) (* a (expt x 2)) (* b x) c)))
