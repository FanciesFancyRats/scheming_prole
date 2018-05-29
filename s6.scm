(define e-time
  (lambda (t ampm)
    (cond
      ((= 12 t) 
       (if (equal? ampm 'am) 0 12))
      ((equal? ampm 'am) t)
      (else (+ t 12)))))

(define a-time
  (lambda (t)
    (cond
      ((= 0 t) (word '12 '" " 'am))
      ((= 12 t) (word '12 " " 'pm))
      ((> 12 t) (word t 'am))
      (#t (word (- t 12) '" " 'pm)))))

(define type-of
  (lambda (x)
    (cond
      ((boolean? x) 'Boolean)
      ((number? x) 'Number)
      ((list? x) 'Sentence)
      ((word? x) 'Word))))

(define teen?
  (lambda (x)
    (if (and (> x 12) (< x 20)) #t #f)))

(define indef-article
  (lambda (x)
    (if (member (first x) '(a e i o))
      (se 'an x)
      (se 'a x)))) 

(define this-many
  (lambda (x thing)
    (if (> x 1) (se  x (plural thing)) (se x thing))))

(define plural
  (lambda (thing)
    (word thing 's)))

(define sort2
  (lambda (a b)
    (if (> a b) (se a b) (se b a))))

(define days31?
  (lambda (m)
    (if 
      (or 
	(and (not (even? m)) (< m 8)) 
	(and (> m 7) (even? m))
	)
      #t #f) 
    ))

;; 31 days: 01 03 05 07 08 10 12
(define valid-date?
  (lambda (m d y)
    (if (> d 31) #f
	(if (> m 12) #f 
    	  (if 
	    (not 
	      (or (and (= m 2) (= d 29) (leap-year? y))(> 2 1) )))))))

(define divisable?
  (lambda (a b)
    (if (= (modulo a b) 0) #t #f)))

(define leap-year?
  (lambda (y)
    (cond
      ( (and (divisable? y 4) (not (divisable? y 100)) ) #t)
      ( (and (divisable? y 4) (divisable? y 100) (divisable? y 400) ) #t)
      (else #f))))


	 
