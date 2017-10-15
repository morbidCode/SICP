(define (square-tree tree)
(cond ((null? tree) '())
((not (pair? tree)) (* tree tree))
(else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(define (square-tree tree)
(map (lambda (subtree) (if (pair? subtree) (square-tree subtree) (* subtree subtree)))
tree))
