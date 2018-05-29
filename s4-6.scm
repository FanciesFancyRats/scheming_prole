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

(define x-to-n
  (lambda (x n)
	(cond
	((< n 1) x)
	(else (x-to-n (* x 10)(- n 1))))))
(define x-to-n-neg
  (lambda (x n)
	(cond
	((> n -1) x)
	(else (x-to-n-neg (/ x 10)(+ n 1)))))) 
(define sci-co-p
  (lambda (x)
	(cond
	((not (= (modulo x 10) 0)) x)
	(else (sci-co-p (/ x 10))))))
(define sci-co-n
  (lambda (x)
	(cond
		((> (log10 x) 1) floor(x))
		(else (sci-co-n (* x 10))))))
(define sci-co
	(lambda (x)
		(cond
		((> x 0) (sci-co-p x))
		((< x 1) (sci-co-n x))
		(else 0))))
(define scientific
	(lambda (x n)
		(cond
		((> n 0) (x-to-n x n))
		((< n 0) (x-to-n-neg x n))
		(else x))))
(define sci-exp
  (lambda (x)
    (cond
      ((> x 1) (sci-exp-p x 1))
      ((< x 1) (sci-exp-n x 1))
      (else 0))))

(define sci-exp-p
  (lambda (x n)
    (cond
      ((< (log10 x) 1) (- n 1))
      (else (sci-exp-p (/ x 10) (+ n 1))))))

(define sci-exp-n
  (lambda (x n)
    (cond
      ((> (log10 x) 0) (* n -1))
      (else (sci-exp-n (* x 10) (+ n 1))))))
