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

;(define (hang-letters letter guesses)
;  (if (member? letter guesses) letter '-))

(define (hang wrd guesses)
  (se (every (lambda (letter) (if (member? letter guesses) letter '-)) wrd)))

(define hang2 
  (lambda (wrd guesses)
    (define hang-letters
      (lambda (letter)
	(if
	  (member? letter guesses) letter '-)))
    (se
      (every hang-letters wrd))))
;;interesting...

(define common-words
  (lambda (sent1 sent2)
    (define in-both?
      (lambda (wrd)
	(if (member? wrd sent2) #t #f)))
    (se (keep in-both? sent1))))

