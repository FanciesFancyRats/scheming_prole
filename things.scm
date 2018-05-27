(define square
  (lambda (n)
    (* n n)))

(define reciprocal
  (lambda (n)
    (if (= n 0)
      "division by 0"
      (/ 1 n))))
