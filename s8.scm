(define (even-count? wd) (even? (count wd)))

(define alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(define beatles '(John Paul George Roingo))

(define (choose-b pred)
  (keep pred '(John Paul George Ringo)))

(define (trans-b fn)
  (every fn '(John Paul George Ringo)))

(define (get-letter-number ltr)
  (get-letter-number-iter ltr 1 alphabet))

(define (get-letter-number-iter ltr n alphabet)
  (if (equal? (first alphabet) ltr) n
    (get-letter-number-iter ltr (+ n 1) (bf alphabet))))

(define alpha-words '(anguish blood cryptic destiny existential fevor galaxy heavensent indignation judgement knife lament momentomori now obligation perpetual quintessential remember starscape terror unimaginable variable witness xenomorphic yielding zealotry))


(define (get-alpha-word n)
  (get-alpha-word-iter n 1 alpha-words))

(define (get-alpha-word-iter n count lst)
  (if (= n count) (first lst)
    (get-alpha-word-iter n (+ count 1) (bf lst))))

(define (get-word ltr)
  (get-alpha-word (get-letter-number ltr)))

(define (words wrd)
  (every get-word wrd))

(define (letter-count wrd)
 (accumulate + (every count wrd)))

(define (exaggerate-word wrd)
  (cond
    ((number? wrd) (+ wrd 3))
    ((equal? wrd 'good) 'great)
    ((equal? wrd 'bad) 'terrible)
    (else wrd)))

(define (exaggerate-sent sent)
  (every exaggerate-word sent))

(define (true-for-all pred sent)
  (equal? sent (keep pred sent)))

(define (grade-modify x)
  (cond
    ((equal? x '+) .33)
    ((equal? x '-) -0.33)
    (else 0)))

(define (grade-base ltr)
  (cond
    ((equal? ltr 'a) 4)
    ((equal? ltr 'b) 3)
    ((equal? ltr 'c) 2)
    ((equal? ltr 'd) 1)
    ((equal? ltr 'f) 0)))

(define (grade-point wrd)
  (+ (grade-base (first wrd)) (grade-modify (bf wrd))))

(define (gpa sent)
  (/ (accumulate + (every grade-point sent)) (count sent)))

(define (is-um? wrd)
  (if (equal? wrd 'um) #t #f))

(define (count-ums sent)
  (count (keep is-um? sent)))

(define (letter-to-number ltr)
  (cond
    ((member? ltr '(a b c)) 2)
    ((member? ltr '(d e f)) 3)
    ((member? ltr '(g h i)) 4)
    ((member? ltr '(j k l)) 5)
    ((member? ltr '(m n o)) 6)
    ((member? ltr '(p q r s)) 7)
    ((member? ltr '(t u v)) 8)
    ((member? ltr '(w x y z)) 9)
    (else 0)))

(define (phone-unspell wrd)
  (every letter-to-number wrd))

(define (subword wrd start finish)
  ((repeated bl 
	     (- 
	       (count wrd) 
	       finish)) 
   ((repeated bf (- start 1)) wrd)))
