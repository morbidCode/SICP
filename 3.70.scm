(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
(let ((s1car (stream-car s1))
(s2car (stream-car s2)))
           (if (< (weight (car s1car) (cadr s1car)) (weight (car s2car) (cadr s2car)))
                  (cons-stream 
                   (stream-car s1) 
                   (merge-weighted (stream-cdr s1) 
                          s2 weight))
                  (cons-stream 
                   (stream-car s2) 
                   (merge-weighted s1 
                          (stream-cdr s2) weight)))))))

(define (weighted-pairs s t weight)
(let ((first-stream (stream-map (lambda (x) (list (stream-car s) x)) t))
(next-stream (lambda () (weighted-pairs (stream-cdr s) (stream-cdr t) weight))))
(let ((pair1 (stream-car first-stream))
(pair2 (stream-car (stream-cdr first-stream)))
(rest-pairs (stream-cdr (stream-cdr first-stream))))
(if (< (weight (car pair1) (cadr pair1)) (weight (car pair2) (cadr pair2)))
(cons-stream pair1 (merge-weighted (cons-stream pair2 rest-pairs) (next-stream) weight))
(cons-stream pair2 (merge-weighted (cons-stream pair1 rest-pairs) (next-stream) weight))))))

1.
(define pairs1 (weighted-pairs integers integers (lambda (i j) (+ i j))))

2.
(define non-divisible-integers (stream-filter (lambda (i) (or (not (= (remainder i 2) 0)) (not (= (remainder i 3) 0)) (not (= (remainder i 5) 0)))) integers))
(define pairs2 
(weighted-pairs non-divisible-integers non-divisible-integers 
(lambda (i j) (+ (* 2 i) (* 3 j) (* 5 i j)))))
