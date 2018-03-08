(define (make-table)
(let ((table (list '*table*)))

(define (last-key? keys) (null? (cdr keys)))
(define (table? record) (equal? (cadr record) '*table*))

(define (assoc key records)
(cond ((null? records) false)
((equal? key (caar records)) 
(car records))
(else (assoc key (cdr records)))))

(define (lookup keys)
(define (lookup-iter keys table)
(if (null? keys)
false
(let ((record (assoc (car keys) (cdr table))))
(if record
(if (last-key? keys)
(if (table? record) false (cddr record))
(if (table? record) (lookup-iter (cdr keys) (cdr record)) false))
false))))
(lookup-iter keys table))

(define (insert! keys value)
(define (insert-new! keys value)
(let ((key (car keys)))
(if (null? (cdr keys))
(cons key (cons '*value* value))
(cons key (cons '*table* (list (insert-new! (cdr keys) value)))))))

(define (insert-iter! keys value table)
(let ((record (assoc (car keys) (cdr table))))
(if record
(if (last-key? keys)
(if (table? record)
(set-cdr! record (cons (cons '*value* value) (cdr record)))
(set-cdr! record (cons '*value* value)))
(if (table? record)
(insert-iter! (cdr keys) value (cdr record))
(set-cdr! record (cons '*table* (list (insert-new! (cdr keys) value))))))
(set-cdr! table (cons (insert-new! keys value) (cdr table))))))
(insert-iter! keys value table)
'ok)

(define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
