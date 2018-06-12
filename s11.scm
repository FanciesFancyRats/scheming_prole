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

(define (is-um? wrd)
  (if (equal? wrd 'um) #t #f))

(define (count-ums2 sent)
  (cond
    ((null? (bf sent)) (if (is-um? (first sent)) 1 0))
    (else (+ (if (is-um?(first sent)) 1 0) (count-ums2 (bf sent))))))

(define (unspell-letter letter)
  (cond
    ((member? letter 'abc) 2)
    ((member? letter 'def) 3)
    ((member? letter 'ghi) 4)
    ((member? letter 'jkl) 5)
    ((member? letter 'mno) 6)
    ((member? letter 'prs) 7)
    ((member? letter 'tuv) 8)
    ((member? letter 'wxy) 9)
    (else 0)))

(define (phone-unspell wrd)
  (cond
    ((= (count wrd) 1) (unspell-letter wrd))
    (else (word (unspell-letter (first wrd)) (phone-unspell (bf wrd))))))

(define (initials sent)
  (cond
    ((= (count sent) 1) (first(first sent)))
    (else (se (first (first sent)) (initials (bf sent))))))

(define (blastoff n)
  (if (> n 0)
    (se n (blastoff (- n 1)))
    'blastoff))

(define (copies n wrd)
  (if (> n 1)
    (se wrd (copies (- n 1) wrd))
    wrd))

(define (reverser wrd)
  (if (= (count wrd) 1)
    wrd
    (word (last wrd) (reverser (bl wrd)))))

(define (factorial n)
  (if (= n 1) 
    n
    (* n (factorial (- n 1)))))
