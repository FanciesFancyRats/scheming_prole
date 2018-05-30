(define (ger wd)
  (let ((ana (if (vowel? (first wd)) (se 'an x 'is) (se 'a x 'is))))
   (se (ana wd) (ana wd) (if (vowel? (first wd)) 'an 'a) wd))) 

