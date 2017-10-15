(define rand
  (let ((x random-init))
(lambda (symbol)
(if (eq? symbol 'generate)
(begin (set! x (rand-update x))
x)
(lambda (y)
(begin (set! x y)
x))))))

