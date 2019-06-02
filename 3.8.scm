(define f
(let ((changed? false))
(lambda (value)
(if changed?
(begin (set! changed? false)
0)
(begin (set! changed? true)
value)))))
