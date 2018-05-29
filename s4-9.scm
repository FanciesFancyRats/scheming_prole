(define discount
  (lambda (price percent)
    (- price (* price (/ percent 100)))))
