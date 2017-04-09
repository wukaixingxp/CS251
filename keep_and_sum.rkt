#lang racket
(define keep-first-n
  (lambda (n l)
    (if (> n 0)
        (let ([x (keep-first-n (- n 1) (cdr l))])
          (append (list(car l)) x))'())))    
  (keep-first-n 3 '(a b c d e f g h i))

(define sum
  (lambda (l)
    (if (not (null? l))
        (let ([x (sum (cdr l))])
          (+ (car l) x))0)))    
  (sum '(4 5 0 1 3))