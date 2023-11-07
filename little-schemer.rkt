#lang scheme

; atom - это любой НЕ список
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

; lat === list of atoms, список, все элементы которого - атомы
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f)
      )))

; проверяет вхождение a в lat
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat)) (member? a (cdr lat)))))))


(define potato 'potato)

(define caesar 'caesar)

(define coffee 'coffee)


; проверяет удаляет первое вхождение
(define rember
  (lambda (a lat)
    (cond
      ((null? lat)  '())
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat))))
      )))


; (rember coffee '(potato coffee coffee))

; map(head)
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l))))
      )))

;(firsts '((potatoes carrots) (apples) (tea coffee)))

; вставляет справа после первого вхождения
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons (car lat) (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat))))
      )))


;(insertR 'foo 'bar '(coffee bar with whiskey))


; вставляет слева после первого вхождения
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat))))
      )))

;(insertL 'foo 'bar '(coffee bar with whiskey))


; заменяет первое вхождение
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat))))
      )))


; (subst 'foo 'bar '(coffee bar with whiskey))


; заменяет первое вхождение old1 ИЛИ old2
(define subst2
  (lambda (new old1 old2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) old1) (eq? (car lat) old2) ) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old1 old2 (cdr lat))))
      )))


;(subst2 'foo 'bar 'coffee '(coffee bar with whiskey))

; удаляет все вхождения
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat))))
      )))


;(multirember coffee '(coffee potato coffee coffee))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat))))
      )))


;(multiinsertR 'foo 'bar '(bar coffee bar with whiskey bar))



(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat))))
      )))


;(multisubst 'foo 'bar '(coffee bar with whiskey bar))

;(pair? '(coffee))

(define sub1
  (lambda (x)
    (- x 1)))

;(sub1 42)

(define add1
  (lambda (x)
    (+ x 1)))


(define zero?
  (lambda (x)
    (eq? (add1 x) 1) 
    ))

(define ➕
  (lambda (x y)
    (cond
      ((zero? y) x)
      (else (add1 (➕ x (sub1 y))))
      )))

;(➕ 41 1)

(define ➖
  (lambda (x y)
    (cond
      ((zero? y) x)
      (else (sub1 (➖ x (sub1 y))))
      )))


;(➖ 1 41)


(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (➕ (car tup) (addtup (cdr tup))))
      )))

;(addtup '(1 2 3 4 5))


(define ×
  (lambda (x y)
    (cond
      ((zero? y) 0)
      (else (➕ x (× x (sub1 y))))
      )))


;(* 2 5)

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (➕ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2))))
      )))

;(tup+ '(1 2) '(3 4 5))




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
    (cond
      ((zero? y) x)
      (else (× x (↑ x (sub1 y)))))))

;(↑ 1 1)
;(↑ 1 2)
;(↑ 10 2)
;(↑ 2 3)


(define ÷	
  (lambda (x y)
    (cond
      ((< x y) 0)
      (else (add1 (÷ (➖ x y) y))))))
;(÷ 15 4)


(define length	
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))
;(length '(coffee bar with whiskey))



; === at(n + 1)
(define pick	
  (lambda (n lat)
    (cond
      ((= n 1) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
;(pick 4 '(coffee bar with whiskey))



; === excludeAllNumbersFromList
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))
;(no-nums '(coffee 3 bar 5 with whiskey 6))


; === excludeAllNOTNumbersFromList
(define allNums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (allNums (cdr lat))))
      (else (allNums (cdr lat))))))
;(allNums '(coffee 3 bar 5 with whiskey 6))


(define eqan?
  (lambda (x y)
    (cond
      ((and (number? x) (number? y)) (= x y))
      ((or (number? x) (number? y)) #f)
      (else (eq? x y))
      )))
;(define vegetables '(potato carrot garlic))
;(eqan? vegetables vegetables)


; считает количество вхождений
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else (➕ (occur a (cdr lat))
               (cond
                 ((eqan? a (car lat)) 1)
                 (else 0)))))))
;(occur 3 '(coffee 3 bar 3 with whiskey 3))


(define one?
  (lambda (x)
    (= x 1)))
;(one? 1)



; === exclude(n + 1)
(define rempick	
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
;(rempick 4 '(coffee bar with whiskey))


(define concat	
  (lambda (lat1 lat2)
    (cond
      ((null? lat2) lat1)
      ((null? lat1) lat2)
      (else (cons (car lat1) (concat (cdr lat1) lat2))))))

;(concat  '((coffee 4) 4 (bar 4 (with whiskey 4))) '((coffee 4) 4 (bar 4 (with whiskey 4))))

(define consApplyDeepHeadApplyDeepTail
  (lambda (fn lat args)
    (cons (apply fn (append args (list (car lat)))) (apply fn (append args (list (cdr lat)))))))

(define consApplyFlatHeadApplyDeepTail
  (lambda (flat deep lat args)
    (concat (apply flat (append args (list (list (car lat))))) (apply deep (append args (list (cdr lat)))))))

(define consDeepOrFlatHeadDeepTail
  (lambda (flat deep lat args)
    (if (pair? (car lat)) (consApplyDeepHeadApplyDeepTail deep lat args) (consApplyFlatHeadApplyDeepTail flat deep lat args))))

(define consDeepOrFlatHeadDeepTailNonNullish
  (lambda (flat deep lat args)
    (cond
      ((null? lat) '())
      ((pair? (car lat)) (consApplyDeepHeadApplyDeepTail deep lat args))
      (else (consApplyFlatHeadApplyDeepTail flat deep lat args)))))

; === deep rember
(define rember*	
  (lambda (a lat)
    (consDeepOrFlatHeadDeepTailNonNullish rember rember* lat (list a))
     ))
    
(rember* 4 '(foo (coffee 4) 4 (bar 4 (with whiskey 4))))