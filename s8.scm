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

(define alpha-words '(aa bb cc dd ee ff gg hh ii jj kk ll mm nn oo pp qq rr ss tt uu vv ww xx yy zz))

(define (get-alpha-word n)
  (get-alpha-word-iter n 1))

(define (get-apha-word n count)
  (if (= n count)

    ;;;Have this helper return the "letter of that number" 
