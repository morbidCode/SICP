(define (pascals-triangle row col)
  (if (or (= col 0) (= col row))
      1
      (+ (pascals-triangle (- row 1)
                            (- col 1))
          (pascals-triangle (- row 1)
                            col))))

