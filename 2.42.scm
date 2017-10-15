(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerate-interval 
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

  (define empty-board nil)

 (define queen cons)
  (define row car)
  (define col cdr)
  
  (define (adjoin-position new-row k rest-of-queens)
    (accumulate cons
                (list (queen new-row k))
                rest-of-queens))
  
  (define (safe? k positions)
    (define new (list-ref positions (- k 1)))
    (define (compare lst)
      (let ((current (car lst)))
        (cond ((null? (cdr lst)) #t)
              ((or (= (row new) (row current))
                   (= (abs (- (row new) (row current)))
                      (- (col new) (col current))))
               #f)
              (else (compare (cdr lst))))))
    (compare positions))
  