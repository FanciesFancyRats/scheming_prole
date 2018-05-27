;(define listofprimes
;  (lambda (n limit primes)
;    (cond
;      ((n 
(define alist '(1 2 3 4))

(define memeber
  (lambda (a b)
    ;(define c (cdr b))
    (cond
      ((null? b) #f )
      ((= (car b) a) #t)
      (else (memeber a (cdr b)) ) )))

(define add-until
  (lambda (n adder limit numbers)
    (cond
      ((> n limit) numbers)
      ((member n numbers) (add-until (+ n adder) adder limit numbers))
      (else (add-until (+ n adder) adder limit (cons n numbers))))))

(define sieve-iter
  (lambda (n limit notPrimes primes)
    (cond
	((> n limit) primes)
	((= n 2) (sieve-iter (+ n 1) limit (add-until n n limit notPrimes) (cons n primes)))
	;((and (= (modulo n 2) 0) (> n 2)) (sieve (+ n 1) limit notPrimes primes))
	((member n notPrimes) (sieve-iter (+ n 1) limit notPrimes primes))
	(else (sieve-iter (+ n 2) limit (add-until n n limit notPrimes) (cons n primes))))))

(define sieve
  (lambda (limit)
    (sieve-iter 2 limit '() '())))

;(define is-multiple
;  (lambda (n listn)
(define list8 '(2 2 2))
(define listnumbers '(1 2 3 4 5 6 7 8 9))


(define multiply-list
  (lambda (n listn product)
    (cond
      ((null? listn) (* n product))
      (else (multiply-list (car listn) (cdr listn) (* n product))))))

(define iter-list
  (lambda (n limit alist)
    (cond 
      ((> n limit) (car alist))
      (else (iter-list (+ n 1) limit (cdr alist))))))

(define testList '(e f g h))

(define (combinations size set)
  (cond ((= size 0) '(()))
	((empty? set) '())
	(else (append (prepend-every ( first set)
				     (combinations (- size 1)
						   (butfirst set)))
		      (combinations size (butfirst set ))))))

(define (prepend-every item lst)
   (map (lambda (choice) (se item choice)) lst))
