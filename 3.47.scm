1.
(define (make-semaphore n)
  (let ((mutex (make-mutex))
(number-of-processes 0))

(define (acquire)
(mutex 'acquire)
(if (< number-of-processes n)
(begin (set! number-of-processes (+ number-of-processes 1))
(mutex 'release))
(begin (mutex 'release)
(acquire))))

(define (release)
(mutex 'acquire)
(set! number-of-processes (- number-of-processes 1))
(mutex 'release))

    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire))
            ((eq? m 'release) (release))))
    the-semaphore))

2.
(define (make-semaphore n)
  (let ((cell (list false))
(number-of-processes 0))

(define (acquire)
(if (test-and-set! cell)
(acquire)
(if (< number-of-processes n)
(begin (set! number-of-processes (+ number-of-processes 1))
(clear! cell))
(begin (clear! cell)
(acquire)))))

(define (release)
(if (test-and-set! cell)
(release)
(begin (set! number-of-processes (- number-of-processes 1))
(clear! cell))))

    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire))
            ((eq? m 'release) (release))))
    the-semaphore))
