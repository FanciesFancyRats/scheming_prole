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

(define (appear? wrd sent)
  (member? wrd sent))

(define (appearances wrd sent)
  (count (keep (lambda (x) (member? wrd sent)) wrd))) ;; need this to be a function, honestly not sure why this one is so tricky?j 
