(define (square x) (* x x))

(define tolerance 0.00001)

(define (average x y) 
  (/ (+ x y) 2))

(define (iterative-improve good-enough? improve-guess)
  (define (try guess)
    (let ((next (improve-guess guess)))
      (if (good-enough? guess next)
          next
          (try next))))
  try)

(define (fixed-point f x)
((iterative-improve 
(lambda (v1 v2) (< (abs (- v1 v2)) tolerance))
 f) x))

(define (sqrt-iter x first-guess)
((iterative-improve 
(lambda (guess next) (< (abs (- (square guess) x)) 0.001))
 (lambda (guess) (average guess (/ x guess)))) first-guess))

(define (sqrt x)
(sqrt-iter x 1.0))
