(define (memetext wrd)
  (if (= (count wrd) 1)
    (se wrd)
    (se wrd (memetext (bl wrd)) wrd))) 

(define (pigl wrd)
  (if (member? (first wrd) '(a e i o u))
    (word wrd 'ay)
    (pigl (word (bf wrd)(first wrd)))))
(define (explode-recurse wrd lst)
  (if (= (count wrd) 0)
    lst
    (explode-recurse (bl wrd) (cons (last wrd) lst))))


(define (explode wrd)
  (explode-recurse wrd ()))
 
(define (pairs wrd)
  (pairs-recurse wrd ()))

(define (pairs-recurse wrd lst)
  (if (= (count wrd) 1)
    lst
    (pairs-recurse (bl wrd) (se (word  (last (bl wrd)) (last wrd)) lst))))

(define (explode2 wrd)
  (if (empty? wrd)
    '()
    (se (first wrd) (explode2 (bf wrd)))))

(define (pairs2 wrd)
  (if (< (count wrd) 2)
    '()
    (se (word (first wrd) (first (bf wrd))) (pairs2 (bf wrd)))))

(define (all-true? lst pred)
  (cond
    ((empty? lst) #t)
    ((pred (first lst)) (all-true? (bf lst) pred))
    (else #f)))

(define (count-ums sent)
  (cond
    ((number? (first sent)) (first sent))
    ((and (equal? 'um (first sent)) (number? (last sent))) (count-ums (se (bf (bl sent)) (+(last sent) 1))))
    ((and (equal? 'um (first sent)) (not (number? (last sent)))) (count-ums (se (bf sent) 1)))
    ((and (not (equal? 'um (first sent))) (not (number? (last sent))) (count-ums (se (bf sent) 0))))
    (else (count-ums (bf sent)))))
