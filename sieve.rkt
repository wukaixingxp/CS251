(define seq
  (lambda (start stop)
    (if (> start stop)
        #f
        (cons start
            (lambda () (seq (+ start 1) stop))))))

(define inf-seq
  (lambda (start)
        (cons start
            (lambda () (inf-seq (+ start 1))))))

; return the first nth numbers in the lazy-list
(define (first-n lazy-list n)
  (if (zero? n)'()
        (cons (car lazy-list) (first-n ((cdr lazy-list)) (- n 1)))))

; return nth number in the lazy-list
(define(nth lazy-list n)
  (cond ((boolean? ((cdr lazy-list)))#f)
        ((eq? n 1) (car lazy-list))
        (else (nth ((cdr lazy-list)) (- n 1)))))

; Filter for lazy-list
(define (is-multiples val n)
  (if (integer? (/ val n)) #t #f))

(define (filter-multiples lazy-list n)
    (if (boolean? lazy-list)
           #f
          (if (is-multiples (car lazy-list) n)
              (filter-multiples ((cdr lazy-list)) n)
               (cons (car lazy-list)  (lambda ()(filter-multiples ((cdr lazy-list)) n))))))


;Sieve of Eratosthenes
(define (primes)
  (sieve(inf-seq 2)))

(define (sieve lazy-list)
  (if (boolean? lazy-list)
           #f
        (cons (car lazy-list) (lambda() (sieve(filter-multiples lazy-list (car lazy-list)))))))
   
;Helper function that convert a countable lazy-list to list for debugging
(define (unlazy lazy-list)
  (if (boolean? lazy-list) '()
      (cons (car lazy-list) (unlazy ((cdr lazy-list))))))

;Test cases:
(first-n (inf-seq 2) 5)
(nth (inf-seq 1) 5)
(nth (seq 1 3) 5)
(unlazy(seq 1 5))
(unlazy(filter-multiples(seq 1 10) 3))
(first-n (primes) 20)
