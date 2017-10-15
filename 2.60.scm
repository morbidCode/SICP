(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
(cons x set))

(define (union-set set1 set2)
(append set1 set2))

(define (remove lst x)
(define (aux acc rest)
(cond ((null? rest) acc)
((eq? (car rest) x) (append acc (cdr rest)))
(else (aux (adjoin-set (car rest) acc) (cdr rest)))))
(aux '() lst))

(define (intersection-set set1 set2)
(cond ((or (null? set1) (null? set2)) '())
((element-of-set? (car set1) set2)
(cons (car set1)
(intersection-set (cdr set1) (remove set2 (car set1)))))
(else (intersection-set (cdr set1) set2))))
