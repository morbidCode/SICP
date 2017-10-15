(define (sum-of-squares a b c)
(if (> a b) (+ (square a) (square (if (> b c) b c)))
(+ (square b) (square (if (> a c) a c)))))
