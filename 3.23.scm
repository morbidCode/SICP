(define (make-item prev-ptr-item value next-ptr-item) (cons prev-ptr-item (cons value next-ptr-item)))

(define (prev-ptr-item item) (car item))
(define (value item) (car (cdr item)))
(define (next-ptr-item item) (cdr (cdr item)))

(define (set-prev-ptr-item! item ptr) (set-car! item ptr))
(define (set-value! item value) (set-car! (cdr item) value))
(define (set-next-ptr-item! item ptr) (set-cdr! (cdr item) ptr))

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
      (value (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an 
              empty deque" deque)
      (value (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-item (make-item '() item (front-ptr deque))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-item)
           (set-rear-ptr! deque new-item)
           deque)
          (else (set-prev-ptr-item! (front-ptr deque) new-item)
                (set-front-ptr! deque new-item)
                deque))))

(define (rear-insert-deque! deque item)
  (let ((new-item (make-item (rear-ptr deque) item '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-item)
           (set-rear-ptr! deque new-item)
           deque)
          (else (set-next-ptr-item! (rear-ptr deque) new-item)
                (set-rear-ptr! deque new-item)
                deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE! called with 
                 an empty deque" deque))
((null? (next-ptr-item (front-ptr deque)))
(set-front-ptr! deque '())
(set-rear-ptr! deque '()))
        (else (set-front-ptr! deque (next-ptr-item (front-ptr deque)))
(set-prev-ptr-item! (front-ptr deque) '())
              deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE! called with 
                 an empty deque" deque))
((null? (prev-ptr-item (rear-ptr deque)))
(set-front-ptr! deque '())
(set-rear-ptr! deque '()))
        (else (set-rear-ptr! deque (prev-ptr-item (rear-ptr deque)))
(set-next-ptr-item! (rear-ptr deque) '())
              deque)))
