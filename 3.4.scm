(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
(define (call-the-cops x) "fuck the cops")
(let ((counter 0))
  (lambda (user-password m)
(if (eq? user-password password)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: 
                 MAKE-ACCOUNT" m)))
(if (= counter 7)
call-the-cops
(begin (set! counter (+ counter 1))
(error "Incorrect password")))))))

