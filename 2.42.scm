(define empty-board nil)

(define (adjoin-position new-row k rest-of-queens)
(cons new-row rest-of-queens))

(define (safe? k positions)
(if (null? positions)
true
(let ((new-queen (car positions)))
(define (unique-row? k positions)
(cond ((null? positions) true)
((= (car positions) new-queen) false)
((= (abs (- (car positions) new-queen)) k) false)
(else (unique-row? (+ k 1) (cdr positions)))))
(unique-row? 1 (cdr positions)))))
