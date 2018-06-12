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

(define (real-word? wrd)
  (if (member wrd '(the of an a for)) #f #t))

(define (real-words sent)
  (if (null? sent)
    '()
    (se (if (real-word? (first sent)) (first sent) '()) (real-words (bf sent)))))

(define (remover wrd sent)
  (if (null? sent)
    '()
    (se (if (equal? wrd (first sent)) '() (first sent)) (remover wrd (bf sent)))))

(define (count sent)
  (if (null? sent)
    0
    (+ 1 (count (bf sent)))))

(define (roman-digit ltr)
  (cond
    ((equal? 'i ltr) 1)
    ((equal? 'v ltr) 5)
    ((equal? 'x ltr) 10)
    ((equal? 'l ltr) 50)
    ((equal? 'c ltr) 100)
    ((equal? 'd ltr) 500)
    ((equal? 'm ltr) 1000)
    (else 0)))

(define (arabic num)
  (if (empty? num)
    0
    (if (> (roman-digit (first num)) (roman-digit (first(bf num)))) 
      (+ (roman-digit (first num)) (arabic (bf num)))
      (- (roman-digit (first num)) (arabic (bf num))))))

