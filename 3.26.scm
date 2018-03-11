(define (key entry) (car entry))
(define (value entry) (cdr entry))
(define (set-value! entry value) (set-cdr! entry value))
(define (make-entry key value)
  (cons key value))

(define (empty? tree) (null? (cdr tree)))
(define (entry tree) (cadr tree))
(define (left-branch tree) (caddr tree))
(define (right-branch tree) (cadddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (make-location) (list '*tree*))

(define (lookup k tree)
(if (empty? tree) #f
(let ((cur (entry tree)))
(cond ((< k (key cur)) (lookup k (left-branch tree)))
((> k (key cur)) (lookup k (right-branch tree)))
(else (value cur))))))

(define (insert! k v tree)
(cond ((empty? tree)
(set-cdr! tree (make-tree (make-entry k v) (make-location) (make-location))))
((< k (key (entry tree))) (insert! k v (left-branch tree)))
((> k (key (entry tree))) (insert! k v (right-branch tree)))
(else (set-value! (entry tree) v))))
