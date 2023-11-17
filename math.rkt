#lang scheme

(provide (all-defined-out) (all-from-out))


(require "numbers-base.rkt")



(define ➕
  (lambda (x y)
    (if
     (zero? y)
     x
     (add1 (➕ x (sub1 y))))
    ))

;(➕ 41 1)

(define ➖
  (lambda (x y)
    (if
     (zero? y)
     x
     (sub1 (➖ x (sub1 y))))
    ))


;(➖ 1 41)

(define >
  (lambda (x y)
    (cond
      ((zero? x) #f)
      ((zero? y) #t)
      (else (> (sub1 x) (sub1 y)))
      )))


;(> 1 1)


(define <
  (lambda (x y)
    (cond
      ((zero? y) #f)
      ((zero? x) #t)
      (else (< (sub1 x) (sub1 y)))
      )))


;(< 1 2)

(define =
  (lambda (x y)
    (not
     (or
      (< x y)
      (> x y)))))

; (= 0 1)

(define ↑	
  (lambda (x y)
    (if
     (zero? y)
     x
     (× x (↑ x (sub1 y))))))

;(↑ 1 1)
;(↑ 1 2)
;(↑ 10 2)
;(↑ 2 3)


(define ÷	
  (lambda (x y)
    (if
     (< x y)
     0
     (add1 (÷ (➖ x y) y)))))
;(÷ 15 4)

(define ×
  (lambda (x y)
    (if
     (zero? y)
     0
     (➕ x (× x (sub1 y))))
    ))


(* 2 5)
