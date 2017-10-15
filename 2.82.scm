(define (lookup fn type-tags)
(if (null? type-tags) (car type-tags)
(let ((cur (car type-tags))
(rest (cdr type-tags)))
(cond ((eq? fn cur) (if (null? rest) cur (lookup (car rest) rest)))
((get-coercion fn cur) (lookup cur rest))
(else #f)))))

(define (apply-generic op . args)
(let ((type-tags (map type-tag args)))
(let ((proc (get op type-tags)))
(if proc
(apply proc (map contents args))
(if (>= (length args) 2)
(let ((convert (lookup (car type-tags) type-tags))
(err (lambda () (error "No method for these types" (list op type-tags)))))
(if convert
(apply apply-generic (cons op (map (lambda (x) (if (eq? convert (type-tag x)) x ((get-coercion (type-tag x) convert) x))) args))) (err)))
(err))))))
