(define (addup nums)
  (if (empty? nums)
    0
    (+ (first nums) (addup (bf nums)))))

(define (acronym sent)
  (if (= (count sent) 1)
    (first (first sent))
    (word (first (first sent))
	  (acronym (bf sent)))))

;(define (badfac n)
;  (if (= (n -1) 1)));would go through 0 so result would be 0?

(define (f sent)
  (if (empty? sent)
    sent
    (se (f(bf sent)) (first sent))))

(define (exaggeratable wrd)
  (cond
    ((number? wrd) (+ 3 wrd))
    ((equal? wrd 'good) 'great)
    ((equal? wrd 'bad) 'terrible)
    (else wrd)))

(define (exaggerate sent)
  (if (null? sent)
     '()
     (se (exaggeratable (first sent)) (exaggerate (bf sent))))) 

(define (grade-value grade)
  (cond
    ((equal? grade 'a) 4)
    ((equal? grade 'b) 3)
    ((equal? grade 'c) 2)
    ((equal? grade 'd) 1)
    ((equal? grade 'f) 0)
    (else 0)))

(define (grade-modifyer mod)
  (cond
    ((equal? mod '+) 0.33)
    ((equal? mod '-) -0.33)
    (else 0)))

(define (grade-point grade)
  (+ (grade-value(first grade)) (grade-modifyer(last grade))))

(define (gpa-add grades)
  (if (null? grades)
    0
   (+ (grade-point(first grades)) (gpa-add (bf grades))) ))

(define (gpa grades)
  (/ (gpa-add grades) (count grades)))

(define (spell-digit digit)
  (item (+ 1 digit)
	'(zero one two three four five six seven eight nine)))

(define (spell-number n)
  (if (empty? (bf n))
   (spell-digit n) 
    (se (spell-digit (first n)) (spell-number (bf n)))))

(define (returns-numbers thing)
  (if (number? thing)
    thing
    '()))
(define (onlynumber sent)
  (if (empty? sent)
    '()
    (se (returns-numbers (first sent)) (onlynumber (bf sent)))))
