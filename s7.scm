(define (ger wd)
  (let ((ana (if (vowel? (first wd)) (se 'an wd 'is) (se 'a wd 'is))))
   (se (ana wd) (ana wd) (if (vowel? (first wd)) 'an 'a) wd))) 

(define (vowel? letter)
  (if (member? letter '(a e i o ui)) #t #f))
