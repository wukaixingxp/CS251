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


(define (first-n lazy-list n)
  (if (zero? n)'()
        (cons (car lazy-list) (first-n ((cdr lazy-list)) (- n 1)))))

(define(nth lazy-list n)
  (cond ((boolean? ((cdr lazy-list)))#f)
        ((eq? n 1) (car lazy-list))
        (else (nth ((cdr lazy-list)) (- n 1)))))

(define (filter-multiples lazy-list n)
(cond ((boolean? ((cdr lazy-list))) #f)
        ((is-multiples (car lazy-list) n) (filter-multiples ((cdr lazy-list)) n))
        (else(my-multiples (car lazy-list) (cdr lazy-list) n))))

(define (is-multiples val n)
  (if (integer? (/ val n)) #t #f))

(define (my-multiples val f n)
    (cond ((boolean? (f)) #f)
        ((is-multiples (car(f)) n) (my-multiples val (cdr (f)) n))
        (else (filter-multiples (cons val f ) n))))


(lambda () (seq (+ start 1) stop)



;(define (filter-multiples lazy-list n)
;(cond ((boolean? ((cdr lazy-list))) #f)
        ;((is-multiples (car lazy-list) n) (filter-multiples ((cdr lazy-list)) n))
        ;(else(my-multiples (car lazy-list) (cdr lazy-list) n))))

;(define (is-multiples val n)
  ;(if (integer? (/ val n)) #t #f))

;(define (my-multiples val f n)
    ;(cond ((boolean? (f)) #f)
        ;((is-multiples (car(f)) n) (my-multiples val (cdr (f)) n))
        ;(else (filter-multiples (cons val f ) n))))
(define (unlazy lazy-list)
  (if (boolean? lazy-list) '()
      (cons (car lazy-list) (unlazy ((cdr lazy-list))))))
;test cases:
;((cdr(seq 1 5)))
;((cdr(inf-seq 2)))
;(first-n (inf-seq 2) 5)
;(nth (inf-seq 1) 5)
;(nth (seq 1 3) 5)
(unlazy(seq 1 5))
(unlazy(filter-multiples(seq 1 7) 2))