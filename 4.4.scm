; normal exps

(define (eval exp env)
  (cond ((self-evaluating? exp) 
         exp)
        ((variable? exp) 
         (lookup-variable-value exp env))
        ((quoted? exp) 
         (text-of-quotation exp))
        ((assignment? exp) 
         (eval-assignment exp env))
        ((definition? exp) 
         (eval-definition exp env))
        ((if? exp) 
         (eval-if exp env))
        ((lambda? exp)
         (make-procedure 
          (lambda-parameters exp)
          (lambda-body exp)
          env))
        ((begin? exp)
         (eval-sequence 
          (begin-actions exp) 
          env))
        ((cond? exp) 
         (eval (cond->if exp) env))
        ((and? exp) 
         (eval-and exp env))
        ((or? exp) 
         (eval-or exp env))
        ((application? exp)
         (apply2 (eval (operator exp) env)
                (list-of-values 
                 (operands exp) 
                 env)))
        (else
         (error "Unknown expression 
                 type: EVAL" exp))))

(define (and? exp)
(tagged-list? exp 'and))
(define (and-no-preds? seq)
(null? seq))
(define (and-last-pred? seq)
(null? (cdr seq)))
(define (and-preds exp)
(cdr exp))
(define (and-first-pred preds)
(car preds))
(define (and-rest-preds preds)
(cdr preds))

(define (eval-and exp env)
(let ((preds (and-preds exp)))
(if (and-no-preds? preds)
true
(and-sequence preds env))))

(define (and-sequence seq env)
(let ((pred (eval2 (and-first-pred seq) env)))
(cond ((and-last-pred? seq) pred)
((true? pred) (and-sequence (and-rest-preds seq) env))
(else false))))

(define (or? exp)
(tagged-list? exp 'or))
(define (or-no-preds? seq)
(null? seq))
(define (or-last-pred? seq)
(null? (cdr seq)))
(define (or-preds seq)
(cdr seq))
(define (or-first-pred preds)
(car preds))
(define (or-rest-preds preds)
(cdr preds))

(define (eval-or exp env)
(let ((preds (or-preds exp)))
(if (or-no-preds? preds)
false
(or-sequence preds env))))

(define (or-sequence seq env)
(let ((pred (eval2 (or-first-pred seq) env)))
(if (or (true? pred) (or-last-pred? seq))
pred
(or-sequence (or-rest-preds seq) env))))

; derived exps

(define (eval exp env)
  (cond ((self-evaluating? exp) 
         exp)
        ((variable? exp) 
         (lookup-variable-value exp env))
        ((quoted? exp) 
         (text-of-quotation exp))
        ((assignment? exp) 
         (eval-assignment exp env))
        ((definition? exp) 
         (eval-definition exp env))
        ((if? exp) 
         (eval-if exp env))
        ((lambda? exp)
         (make-procedure 
          (lambda-parameters exp)
          (lambda-body exp)
          env))
        ((begin? exp)
         (eval-sequence 
          (begin-actions exp) 
          env))
        ((cond? exp) 
         (eval (cond->if exp) env))
        ((and? exp) 
         (eval (and->if exp) env))
        ((or? exp) 
         (eval (or->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values 
                 (operands exp) 
                 env)))
        (else
         (error "Unknown expression 
                 type: EVAL" exp))))

(define (and? exp)
(tagged-list? exp 'and))

(define (and->if exp)
(let ((preds (cdr exp)))
(and-sequence preds)))

(define (and-sequence seq)
(if (null? seq)
'true
(let ((pred (car seq)))
(make-if pred (and-sequence (cdr seq)) pred)))))

(define (or? exp)
(tagged-list? exp 'or))

(define (or->if exp)
(let ((preds (cdr exp)))
(or-sequence preds)))

(define (or-sequence seq)
(if (null? seq)
'false
(let ((pred (car seq)))
(make-if pred pred (or-sequence (cdr seq))))))
