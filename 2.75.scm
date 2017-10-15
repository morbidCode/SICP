(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* ((make-from-mag-ang r a) 'magnitude) (cos ((make-from-mag-ang r a) 'angle))))
          ((eq? op 'imag-part) (* ((make-from-mag-ang r a) 'magnitude) (sin ((make-from-mag-ang r a) 'angle))))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op: 
            MAKE-FROM-MAG-ANG" op))))
  dispatch)
