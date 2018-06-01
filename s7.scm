(define (ger wrd)
  (let ((ana (if (vowel? (first wrd)) 'an 'a)))
    (se ana wrd 'is ana wrd 'is ana wrd))) 


(define (vowel? letter)
  (if (member? letter '(a e i o ui)) #t #f))

(define (test x)
  (let ((thing (sqrt x))
	(thingb (* x x)))
    (se thing thingb)))

(define (piething)
  (let ((pi '(3.14159))
  	(pie '(lemon meringue)))
  (se 'pi 'is pi 'but 'pie 'is pie)))

(define (sup adj wrd)
  (se (word adj 'est) wrd))

(define (sum-sqr a b)
  (let ((+ *)
	(* +))
    (* (+ a a) (+ b b))))
(define (add2 n)
  (+ n 2))

(define (a2e lst)
  (every add2 lst))
