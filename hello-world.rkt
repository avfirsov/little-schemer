#lang scheme

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f)
      )))

(display (lat? '(foo bar)))
