(#%require sicp) ; from https://pkgs.racket-lang.org/package/sicp
(#%require "sicp-concurrency.rkt") ; from https://gist.github.com/soegaard/d32e12d89705c774b71ee78ef930a4bf

; other procedures from the book

(define (square x) (* x x))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (stream-car stream) 
  (car stream))

(define (stream-cdr stream) 
  (force (cdr stream)))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) 
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream 
          (stream-car stream)
          (stream-filter 
           pred
           (stream-cdr stream))))
        (else (stream-filter 
               pred 
               (stream-cdr stream)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream 
       (proc (stream-car s))
       (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (add-streams s1 s2) 
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

; user-defined procedures

(define (prime? n)
(define (even? n) (= (remainder n 2) 0))
  (define (expmod base exp m)
    (define (expmod-iter a base exp)
      (define squareMod (remainder (square base) m))
      (cond ((= exp 0) a)
            ((and (not (= base (- m 1)))
                  (not (= base 1))
                  (= squareMod 1))
             0)
            ((even? exp)
             (expmod-iter
              a
              squareMod
              (/ exp 2)))
            (else
             (expmod-iter
              (remainder (* a base) m)
              base
              (- exp 1)))))
    (expmod-iter
     1
     base
     exp))
  (define (miller-rabin-test n)
    (define (try-it a)
      (if (and (= a 1)
               (> n 2))
          (miller-rabin-test n)
          (= (expmod a (- n 1) n) 1)))
    (try-it (+ 1 (random (- n 1)))))  
  (define (fast-prime? times)
    (cond ((= times 0) true)
          ((miller-rabin-test n)
           (fast-prime? (- times 1)))
          (else false)))
(fast-prime? n))

; exercise 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc 
                    (map stream-cdr 
                         argstreams))))))
