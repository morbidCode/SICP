(define tolerance 0.00001)

(define (compose f g)
(lambda (x) (f (g x))))

(define (repeated f n)
(if (= n 1)
(lambda (x) (f x))
(compose f (repeated f (- n 1)))))

(define (average x y) 
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) 
    (average x (f x))))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))  
(define (try guess)
(display guess)
(newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x tolerance)) (g x))
       tolerance)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) 
            ((deriv g) x)))))

(define (fixed-point-of-transform 
         g transform guess)
  (fixed-point (transform g) guess))

(define (least-expt n)
    (define (least x)
      (if (> (expt 2 x) n) 
          (- x 1) 
          (least (+ x 1))))
    (least 1))
  
  (define (nth-root n)
    (lambda (x) 
      (fixed-point-of-transform 
       (lambda (y) 
         (/ x 
            (expt y (- n 1))))
       (repeated 
        average-damp 
        (least-expt n))
       1.0)))
  
