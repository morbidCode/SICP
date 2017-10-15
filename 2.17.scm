(define (last-pair lst)
(let ((next (cdr lst)))
(if (null? next) lst (last-pair next))))
