(define (intersection-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-list 
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-list 
                          (cdr set1) 
                          set2))
              ((< x2 x1) (intersection-list 
                          set1 
                          (cdr set2)))))))

(define (union-list set1 set2)
(cond ((null? set1) set2)
((null? set2) set1)
(else (let ((a1 (car set1))
(d1 (cdr set1))
(a2 (car set2))
(d2 (cdr set2)))
(cond ((< a1 a2) (cons a1 (union-list d1 set2)))
((> a1 a2) (cons a2 (union-list set1 d2)))
(else (cons a1 (union-list d1 d2))))))))

(define (intersection-set t1 t2)
(list->tree (intersection-list (tree->list-2 t1) (tree->list-2 t2))))

(define (union-set t1 t2)
(list->tree (union-list (tree->list-2 t1) (tree->list-2 t2))))
