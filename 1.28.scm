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
  
  (define (fast-prime? n times)
    (cond ((= times 0) true)
          ((miller-rabin-test n)
           (fast-prime? n (- times 1)))
          (else false)))
