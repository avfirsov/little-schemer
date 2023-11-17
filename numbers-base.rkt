#lang scheme

(require (rename-in scheme [number? number?scheme] [zero? zero?scheme]) )

(define number?
  (lambda (x)
    (number?scheme x)))

(number? 42)


(define zero?
  (lambda (x)
    (zero?scheme x)))


(define sub1
  (lambda (x)
    (- x 1)))

;(sub1 42)


(define add1
  (lambda (x)
    (+ x 1)))
