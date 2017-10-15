(define (unique-triples n)
(flatmap (lambda (i) (flatmap (lambda (j) (map (lambda (k) (list i j k)) (enumerate-interval 1 (- j 1)))) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n)))

(define (triple-sum? pair)
(+ (car pair) (cadr pair) (caddr pair)))

(define (make-triple-sum pair)
  (list (car pair) 
(cadr pair) 
(caddr pair) 
(+ (car pair) (cadr pair) (caddr pair))))


(define (distinct-sum-triples n s)
(map make-triple-sum (filter (lambda (triple) (= (triple-sum? triple) s)) (unique-triples n))))
