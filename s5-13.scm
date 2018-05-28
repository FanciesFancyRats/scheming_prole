(define third
	(lambda (x)
	(first (bf (bf x)))))

(define second
  (lambda (x)
	(first (bf x))))

(define first-two
  (lambda (x)
	(word (first x) (second x))))

(define two-first
  (lambda (wrdA wrdB)
    (word (first wrdA) (first wrdB))))

(define knight
  (lambda (sentA)
    (se '(sir/lady) sentA)))

(define query
  (lambda (x)
    (se (second x) (first x) (bf (bf x)) '(?))))
