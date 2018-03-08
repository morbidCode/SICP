; not a box and pointer diagram because

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define x (cons 1 '()))

(count-pairs (cons 1 (cons 2 x)))

(define y (cons x x))
(count-pairs (cons 1 y))

(count-pairs (cons y y))

(define z (cons 1 (cons 2 (cons 3 '()))))
(set-cdr! (cddr z) z)
(count-pairs z)
