(define (e k)
(+ 2 (cont-frac (lambda (i) 1.0)
(lambda (i)
(let ((mod (remainder i 3)))
(if (= mod 2)
(+ 2 (* mod (/ (- i mod) 3))) 1)))
k)))
