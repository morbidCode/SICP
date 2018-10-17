(define (value-item item) (car item))
(define (prev-ptr-item item) (cdr item))
(define (item? item) (pair? item))
(define (set-prev-ptr-item! item ptr) 
  (set-cdr! item ptr))

(define (make-item value prev-ptr) (cons value prev-ptr))

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) 
  (set-car! deque item))
(define (set-rear-ptr! deque item) 
  (set-cdr! deque item))

(define (empty-deque? deque) 
  (null? (front-ptr deque)))

(define (make-deque) (cons '() '()))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an 
              empty deque" deque)
      (value-item (car (front-ptr deque)))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an 
              empty deque" deque)
      (value-item (car (rear-ptr deque)))))

(define (front-insert-deque! deque item-value)
  (let ((new-pair (cons (make-item item-value '()) '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair))
          (else (set-cdr! new-pair (front-ptr deque))
(set-prev-ptr-item! (car (front-ptr deque)) new-pair)
                (set-front-ptr! deque new-pair)))))

(define (rear-insert-deque! deque item-value)
  (let ((new-pair (cons (make-item item-value '()) '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair))
          (else (set-prev-ptr-item! (car new-pair) (rear-ptr deque))
(set-cdr! (rear-ptr deque) 
                          new-pair)
                (set-rear-ptr! deque new-pair)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE! called with 
                 an empty deque" deque))
        (else (let ((next-ptr (cdr (front-ptr deque))))
(if (item? next-ptr)
(set-prev-ptr-item! (car next-ptr) '())
(set-rear-ptr! deque '()))
(set-front-ptr! deque next-ptr)))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE! called with 
                 an empty deque" deque))
(else (set-rear-ptr! deque (prev-ptr-item (car (rear-ptr deque))))
(let ((rear-ptr (rear-ptr deque)))
(if (item? rear-ptr)
(set-cdr! rear-ptr '())
(set-front-ptr! deque '()))))))
