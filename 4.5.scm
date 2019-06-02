(define (cond-extended? clause) 
  (and (eq? (cadr clause) '=>) (null? (cdddr clause))))
(define (cond-extended-action clause) 
  (caddr clause))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp 
                 (cond-actions first))
                (error "ELSE clause isn't 
                        last: COND->IF"
                       clauses))
(if (cond-extended? first)
(make-if (cond-predicate first)
(list (cond-extended-action first) (cond-predicate first))
(expand-clauses rest))
(make-if (cond-predicate first)
                     (sequence->exp 
                      (cond-actions first))
                     (expand-clauses 
                      rest)))))))
