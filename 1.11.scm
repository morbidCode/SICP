(define (f n)
(if (< n 3)
n
(+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f n) 
(define (f-iter a b c count)
(cond ((< n 0) n)
((= count 0) c)
(else (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
(f-iter 2 1 0 n))

