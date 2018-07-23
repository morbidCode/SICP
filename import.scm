;source: https://stackoverflow.com/questions/9347294/
(require sicp)
(print-as-expression #f)
(#%require r5rs/init)

; source: https://planet.racket-lang.org/display.ss?package=sicp-concurrency.plt&owner=dyoo
(require (planet dyoo/sicp-concurrency:1:2/sicp-concurrency)) 

; procedures

(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
