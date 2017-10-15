(define (element-of-set? x set)
(cond ((null? set) false)
((eq? x (car set)) true)
(else (element-of-set? x (cdr set)))))

(define (encode-symbol char tree)
(if (leaf? tree) '()
(let ((lb (left-branch tree))
(rb (right-branch tree)))
(cond ((element-of-set? char (symbols lb)) (cons 0 (encode-symbol char lb)))
((element-of-set? char (symbols rb)) (cons 1 (encode-symbol char rb)))
(else (error "fuck" char))))))

(define (encode message tree)
(if (null? message)
'()
(append (encode-symbol (car message) tree) (encode (cdr message) tree))))
