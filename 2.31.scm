; unused process referenced in: cs61a

(define (tree-map fn tree)
(map (lambda (subtree) (if (pair? subtree) (tree-map fn subtree) (fn subtree))) tree))
