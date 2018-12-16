(#%require sicp) ; from https://pkgs.racket-lang.org/package/sicp
(print-as-expression #f)
(#%require r5rs/init)
(#%require "sicp-concurrency.rkt") ; from https://gist.github.com/soegaard/d32e12d89705c774b71ee78ef930a4bf

; other procedures from the book

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
