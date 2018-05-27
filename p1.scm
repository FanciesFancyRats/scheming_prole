(define problem1
  (lambda (n sum)
    (cond
      ((> 1 n) sum)
      ((divideby5 n) (problem1 (- n 1)(+ sum n)))
      ((divideby3 n) (problem1 (- n 1)(+ sum n)))
      (else (problem1 (- n 1)(+ sum 0))))))
(define divideby5
  (lambda (n)
    (cond
      ((= 0 (modulo n 5)) #t)
      (else #f))))
(define divideby3
  (lambda (n)
    (cond
      ((= 0 (modulo n 3)) #t)
      (else #f))))
(define count
  (lambda (n sum)
    (cond
      ((> n 999) sum)
      ((divideby3 n) (count (+ n 1)(+ sum n)))
      ((divideby5 n) (count (+ n 1)(+ sum n)))
      (else (count (+ n 1)(+ sum 0))))))

