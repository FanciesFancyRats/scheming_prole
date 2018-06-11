(define (roots a b c)
  ((lambda (discriminants)
     (se (/ (+ (- b) discriminants) (* 2 a))
	 (/ (- (- b) discriminants) (* 2 a))))
   (sqrt (- (* b b) (* 4 a c)))))

(define (who sent)
  (every (lambda (person) (se person sent)) '(pete roger johon keith)))

(define (prepend-every pre sent)
  (every (lambda (wrd) (word pre wrd)) sent))

(define (sentence-version fn)
  (lambda (sent) (every fn sent)))

(define (letterwords ltr sent)
  (keep (lambda (wrd) (if (member? ltr wrd) #t #f)) sent))

(define (hang-letters letter guesses)
  (if (member? letter guesses) letter '-))



(define (appearances wrd sent)
  (count (keep (lambda (sent) (member? wrd sent)) sent)))

(define (unabrev sent1 sent2)
  (se (every (lambda (x) (if (number? x) (item x sent2) x)) sent1)))

(define (first-last sent)
  (keep (lambda (wrd) (equal? (first wrd) (last wrd))) sent))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (substitute sub target sent)
  (se (every (lambda (x) (if (equal? x target) sub x)) sent)))

(define (type-check fn pred)
  (lambda (x) (if (pred x) (fn x) #f)))

(define (aplize fn)
  (lambda (x) (if (sentence? x) (se (every fn x)) (fn x))))

(define (notkeep lst pred)
  (se (every (lambda (x) (if (pred x) x ())) lst)))

