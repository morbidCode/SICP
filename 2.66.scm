(define (lookup key tree-record)
(if (null? tree-record) #f
(let ((cur (entry tree-record)))
(cond ((< key (key cur)) (lookup key (left-branch tree-record)))
((> key (key cur)) (lookup key (right-branch tree-record)))
(else cur)))))
