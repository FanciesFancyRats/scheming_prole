(define (ger wd)
  (let ((ana (if (vowel? (first wd)) 'true 'false)))
   (se (ana wd) (ana wd) (if (vowel? (first wd)) 'an 'a) wd)))
;;
;(define (vowel? letter)
;  (if (member? letter '(a e i o ui)) #t #f))

(define (test x)
  (let ((thing (sqrt x))
	(thingb (* x x)))
    (se thing thingb)))


