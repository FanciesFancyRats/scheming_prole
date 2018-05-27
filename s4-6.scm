(define square
  (lambda (x)
    (* x x)))
(define fourth-1
  (lambda (x)
    (* x x x x)))

(define (fourth-2 x)
  (square (square x)))



(define silly-abs
  (lambda (x)
    (sqrt (square x))))

;(define scientific
;  (lambda (x n

(define x-pwr-10-iter
  (lambda (x n)
    
