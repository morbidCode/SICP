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

(define (tree->list-1 tree)
(if (null? tree) '()
(append (tree->list-1 (left-branch tree))
(cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
(define (copy-to-list tree result-list)
(if (null? tree) result-list
(copy-to-list (left-branch tree) (cons (entry tree) (copy-to-list (right-branch tree) result-list)))))
(copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))

(define (intersection-set t1 t2)
(list->tree (intersection-list (tree->list-2 t1) (tree->list-2 t2))))

(define (union-set t1 t2)
(list->tree (union-list (tree->list-2 t1) (tree->list-2 t2))))
