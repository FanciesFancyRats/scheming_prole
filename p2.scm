(define even??
  (lambda (n)
    (if (= (modulo n 2) 0)
      #t
      #f)))

(define p2
  (lambda (a b sum)
    (cond
      ((> a 4000000) sum)
      ((even?? a) (p2 (+ a b)(+ a 0)(+ sum a)))
      (else (p2 (+ a b)(+ a 0) (+ sum 0))))))
     
