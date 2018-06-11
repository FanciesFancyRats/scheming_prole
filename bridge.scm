(define (card-eval card)
  (cond
    ((equal? (bf card) 'a) 4)
    ((equal? (bf card) 'k) 3)
    ((equal? (bf card) 'q) 2)
    ((equal? (bf card) 'j) 1)
    (else 0)))

(define (high-card-points hand)
  (accumulate + (every card-eval hand)))

(define (count-suit suit hand)
  (accumulate + (every (lambda (x) (if (equal? suit (first x)) 1 0)) hand)))

(define (suits-counts hand)
  (se (count-suit 's hand) (count-suit 'h hand) (count-suit 'c hand) (count-suit 'd hand) ))

(define (suits-dist-points number)
  (cond
    ((= number 0) 3)
    ((= number 1) 2)
    ((= number 2) 1)
    (else 0)))

(define (hand-dist-points hand)
  (accumulate + (every suits-dist-points (suits-counts hand))))

(define (bridge-val hand)
  (+ (hand-dist-points hand) (high-card-points hand)))
