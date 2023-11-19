#lang scheme
(require "math.rkt")
(provide (all-defined-out) (all-from-out))



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
    (if
     (null? lat)
     #f
     (or (equal? a (car lat)) (member? a (cdr lat))))))


(define potato 'potato)

(define caesar 'caesar)

(define coffee 'coffee)


; проверяет удаляет первое вхождение
(define rember
  (lambda (a lat)
    (cond
      ((null? lat)  '())
      ((equal? (car lat) a) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat))))
      )))


; (rember coffee '(potato coffee coffee))

; map(head)
(define firsts
  (lambda (l)
    (if
     (null? l) '() (cons (first (first l)) (firsts (cdr l))))
    ))

;(firsts '((potatoes carrots) (apples) (tea coffee)))

; вставляет справа после первого вхождения
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((equal? (car lat) old) (cons (car lat) (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat))))
      )))


;(insertR 'foo 'bar '(coffee bar with whiskey))


; вставляет слева после первого вхождения
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((equal? (car lat) old) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat))))
      )))

;(insertL 'foo 'bar '(coffee bar with whiskey))


; заменяет первое вхождение
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((equal? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat))))
      )))


; (subst 'foo 'bar '(coffee bar with whiskey))


; заменяет первое вхождение old1 ИЛИ old2
(define subst2
  (lambda (new old1 old2 lat)
    (cond
      ((null? lat) '())
      ((or (equal? (car lat) old1) (equal? (car lat) old2) ) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old1 old2 (cdr lat))))
      )))


;(subst2 'foo 'bar 'coffee '(coffee bar with whiskey))

; удаляет все вхождения
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((equal? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat))))
      )))


;(multirember coffee '(coffee potato coffee coffee))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((equal? (car lat) old) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat))))
      )))


;(multiinsertR 'foo 'bar '(bar coffee bar with whiskey bar))



(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((equal? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat))))
      )))


;(multisubst 'foo 'bar '(coffee bar with whiskey bar))

;(pair? '(coffee))



(define addtup
  (lambda (tup)
    (if
     (null? tup)
     0
     (➕ (car tup) (addtup (cdr tup))))
    ))

;(addtup '(1 2 3 4 5))




(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (➕ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2))))
      )))

;(tup+ '(1 2) '(3 4 5))






(define length	
  (lambda (lat)
    (if
     (null? lat)
     0
     (add1 (length (cdr lat))))))
;(length '(coffee bar with whiskey))



; === at(n + 1)
(define pick	
  (lambda (n lat)
    (if
     (= n 1)
     (car lat)
     (pick
      (sub1 n)
      (cdr lat)))))
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

; сравнивает числа, атомы и списки (по ссылке)
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
    (if
     (null? lat)
     0
     (➕ (occur a (cdr lat))
        (if
         (equal? a (car lat))
         1
         0)))))
;(occur 3 '(coffee 3 bar 3 with whiskey 3))


(define one?
  (lambda (x)
    (= x 1)))
;(one? 1)



; === exclude(n + 1)
(define rempick	
  (lambda (n lat)
    (if
     (one? n)
     (cdr lat)
     (cons
      (car lat)
      (rempick
       (sub1 n)
       (cdr lat))))))
;(rempick 4 '(coffee bar with whiskey))



(define consApplyDeepHeadApplyDeepTail
  (lambda (fn lat args #:mode [mode 'list])
    (cond
      ((equal? mode 'list) (cons (apply fn (append args (list (car lat)))) (apply fn (append args (list (cdr lat))))))
      ((equal? mode 'plus) (+ (apply fn (append args (list (car lat)))) (apply fn (append args (list (cdr lat))))))
      ((equal? mode 'and) (and (apply fn (append args (list (car lat)))) (apply fn (append args (list (cdr lat))))))
      ((equal? mode 'or) (or (apply fn (append args (list (car lat)))) (apply fn (append args (list (cdr lat))))))
      )))

(define consApplyFlatHeadApplyDeepTail
  (lambda (flat deep lat args #:mode [mode 'list])
    (cond
      ((equal? mode 'list) (append (apply flat (append args (list (list (car lat))))) (apply deep (append args (list (cdr lat))))))
      ((equal? mode 'plus) (+ (apply flat (append args (list (list (car lat))))) (apply deep (append args (list (cdr lat))))))
      ((equal? mode 'and) (and (apply flat (append args (list (list (car lat))))) (apply deep (append args (list (cdr lat))))))
      ((equal? mode 'or) (or (apply flat (append args (list (list (car lat))))) (apply deep (append args (list (cdr lat))))))
      )))

(define consApplyDeepOrFlatHeadApplyDeepTail
  (lambda (flat deep lat args #:mode [mode 'list])
    (if
     (pair? (car lat))
     (consApplyDeepHeadApplyDeepTail deep lat args #:mode mode)
     (consApplyFlatHeadApplyDeepTail flat deep lat args #:mode mode))))

(define consApplyDeepOrFlatHeadApplyDeepTailWithNullishFallback
  (lambda (flat deep nullFallback lat args #:mode [mode 'list])
    (if 
     (null? lat) nullFallback (consApplyDeepOrFlatHeadApplyDeepTail flat deep lat args #:mode mode))))
  

; === deep rember
(define rember*	
  (lambda (a lat)
    (consApplyDeepOrFlatHeadApplyDeepTailWithNullishFallback rember rember* '() lat (list a))
    ))
    
;(rember* 4 '(foo (coffee 4) 4 (bar 4 (with whiskey 4))))

(define insertR*	
  (lambda (new old lat)
    (consApplyDeepOrFlatHeadApplyDeepTailWithNullishFallback insertR insertR* '() lat (list new old))
    ))
    
;(insertR* "new item" 4 '(foo (coffee 4) 4 (bar 4 (with whiskey 4))))

(define occur*	
  (lambda (a lat)
    (consApplyDeepOrFlatHeadApplyDeepTailWithNullishFallback occur occur* 0 lat (list a) #:mode 'plus)
    ))

;(occur*  4 '(4 foo 4 (coffee 4) 4 (bar 4 (with whiskey 4))))

(define subst*	
  (lambda (new old lat)
    (consApplyDeepOrFlatHeadApplyDeepTailWithNullishFallback subst subst* '() lat (list new old))
    ))

;(subst* 'bazz 4 '(4 foo 4 (coffee 4) 4 (bar 4 (with whiskey 4))))


(define insertL*	
  (lambda (new old lat)
    (consApplyDeepOrFlatHeadApplyDeepTailWithNullishFallback insertL insertL* '() lat (list new old))
    ))
    
;(insertL* "new item" 4 '(foo (coffee 4) 4 (bar 4 (with whiskey 4))))

(define member*?	
  (lambda (a lat)
    (consApplyDeepOrFlatHeadApplyDeepTailWithNullishFallback member? member*? #f lat (list a) #:mode 'or)
    ))

;(member*?  4 '(foo (coffee) 3 (bar 2 (with whiskey 4))))

(define leftmost	
  (lambda (lat)
    (if
     (atom? (car lat)) (car lat) (leftmost (car lat)))))

;(leftmost '(((bar 2 (with whiskey 4))) foo (coffee) 3))



(define eqlist?	
  (lambda (lat1 lat2)
    (cond
      ((and (null? lat1) (null? lat2)) #t)
      ((or (null? lat1) (null? lat2)) #f)
      (else (and (equal? (car lat1) (car lat2)) (eqlist? (cdr lat1) (cdr lat2)))))))
;(eqlist '(((bar 2 (with whiskey 4))) foo (coffee) 3) '(((bar 2 (with whiskey 4))) foo (coffee) 3))
;(eqlist '(2 4 3) '(2 4))
;(eqlist '(2 4) '(2 4 3))
;(eqlist '(2 4 3) '(2 4 3))

;наиболее общая сравнялка, сравнивает все, даже списки (структурно и глубоко)
(define equal?	
  (lambda (x y)
    (if
     (and (pair? x) (pair? y))
     (eqlist? x y)
     (eqan? x y))))
;(equal? '(((bar 2 (with whiskey 4))) foo (coffee) 3) '(((bar 2 (with whiskey 4))) foo (coffee) 3))
;(equal? '(2 4 3) '(2 4))
;(equal? '(2 4) '(2 4 3))
;(equal? '(2 4 3) '(2 4 3))
;(equal? 'foo 'foo)
;(equal? 'foo '())
;(equal? 4 42)

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col '() 0 0))
      ((eq? oldL (car lat)) (multiinsertLR&co
                             new
                             oldL
                             oldR
                             (cdr lat)
                             (lambda (newLat visitedL visitedR)
                               (col (cons new (cons oldL newLat)) (add1 visitedL) visitedR))))
      ((eq? oldR (car lat))(multiinsertLR&co
                            new
                            oldL
                            oldR
                            (cdr lat)
                            (lambda (newLat visitedL visitedR)
                              (col (cons oldR (cons new newLat)) visitedL (add1 visitedR)))))
      (else (multiinsertLR&co
             new
             oldL
             oldR
             (cdr lat)
             (lambda (newLat visitedL visitedR)
               (col (cons (car lat) newLat) visitedL visitedR))))
      )))

(define lat '(1 2 3 foo shouldBeAppended shouldBePrepended bar baz shouldBePrepended))
(define (tap . args)
  (display args))
;(multiinsertLR&co 'added 'shouldBePrepended 'shouldBeAppended lat tap)

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) (col '() '()))
      ((eq? a (car lat)) (multirember&co
                          a
                          (cdr lat)
                          (lambda (cutted visited)
                            (col cutted (cons a visited)))))
      (else (multirember&co
             a
             (cdr lat)
             (lambda (cutted visited)
               (col (cons (car lat) cutted) visited))))
      )))
;(multirember&co 'shouldBePrepended lat tap)


(define even?
  (lambda (n)
    (cond
      ((zero? n) #t)
      ((one? n) #f)
      (else (even? (➖ n 2)))
      )))

;(even? 4)

(define evens-only&co
  (lambda (lat col)
    (cond
      ((null? lat) (col '() 1 0))
      ((even? (car lat)) (evens-only&co
                          (cdr lat)
                          (lambda (evensOnly evensProduct oddSum)
                            (col (cons (car lat) evensOnly) (× evensProduct (car lat)) oddSum))))
      (else (evens-only&co
             (cdr lat)
             (lambda (evensOnly evensProduct oddSum)
               (col evensOnly evensProduct (➕ oddSum (car lat))))))
      )))



;(evens-only&co '(1 2 3 4 5 6 7 8 9 10) tap)


(define keep-looking
  (lambda (ind lat)
    (if (number? (pick ind lat)) (keep-looking (pick ind lat) lat) (pick ind lat))))

(define looking
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((number? (car lat)) (equal? a (keep-looking (car lat) lat)))
      (else (equal? (car lat) a)))))

;(looking 'bar '(2 3 6 foo bar 5))

(define mathOps
  '(+ ↑ ×))

(define isOp?
  (lambda (a)
    (member? a mathOps)))

(define getOp
  (lambda op
    (cond
      ((equal? op '(+)) ➕)
      ((equal? op '(×)) ×)
      ((equal? op '(↑)) ↑))))



(define operator
  (lambda (aexp)
    (car (cdr aexp))))

(define left-operand
  (lambda (aexp)
    (car aexp)))


(define right-operand
  (lambda (aexp)
    (car (cdr (cdr aexp)))))


(define numbered?
  (lambda (aexp)
    (or
     (and (atom? aexp) (number? aexp))
     (and (numbered? (left-operand aexp)) (numbered? (right-operand aexp)) (isOp? (operator aexp))))))

;(numbered? '(1 + (4 + 21)))


'((define value
    (lambda (aexp)
      (if
       (atom? aexp) aexp
       ((getOp (operator aexp)) (value (left-operand aexp)) (value (right-operand aexp)))))))

;(value '(1 + (4 ↑ 2)))



(define set?
  (lambda (lat)
    (if
     (null? lat) #t
     (and (not (member? (car lat) (cdr lat))) (set? (cdr lat))))))

;(set? '(1 2 3 4 5 6 1))


(define identity
  (lambda (x)
    x))

(define _reverse
  (lambda (lat cb)
    (cond
      ((null? lat) (cb '()))
      ((null? (cdr lat)) (cb lat))
      (else (_reverse
             (cdr lat)
             (lambda (acc)
               (cb (append acc (cons(car lat) '())))
               ))))))

(define reverse
  (lambda (lat)
    (_reverse lat identity)))

;(reverse '(1 2 3 4 5 6 1))
;(reverse '(1 ))
;(reverse '())

;((lambda lat lat)'(1 2 3))


;удаляющий первые вхождения
(define _makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat)) (_makeset(cdr lat)))
      (else (cons (car lat) (_makeset (cdr lat)))
            ))))

;(_makeset '(1 2 3 4 5 6 1))

;удаляющий последующие
(define makeset
  (lambda (lat)
    (reverse (_makeset (reverse lat)))))

;(makeset '(1 2 3 4 5 6 1 1 1))


(define makesetMultirember
  (lambda (lat)
    (if (null? lat) '() (cons (car lat) (multirember (car lat) (cdr lat))))))

;(makesetMultirember '(1 2 3 4 5 6 1 1 1))

(define subset?
  (lambda (set1 set2)
    (if
     (null? set1)
     #t
     (and (member? (car set1) set2) (subset? (cdr set1) set2))
     )))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))


(define intersect?
  (lambda (set1 set2)
    (and
     (not (null? set1))
     (or  (member? (car set1) set2) (intersect? (cdr set1) set2)))))

;(intersect? '(apple juice bread) '(whiskey wine beer))
;(intersect? '(apple juice bread) '(whiskey wine beer juice))

(define intersect
  (lambda (set1 set2)
    (if
     (null? set1)
     '()
     (append
      (if (member? (car set1) set2) (cons (car set1) '()) '())
      (intersect (cdr set1) set2)))))

;(intersect '(apple juice bread) '(whiskey wine beer juice))

(define union
  (lambda (set1 set2)
    (makeset (append set1 set2))))
;(union '(apple juice bread) '(whiskey wine beer juice))

(define xxx
  (lambda (set1 set2)
    (if
     (null? set2) set1
     (xxx (multirember (car set2) set1) (cdr set2)))))

;(xxx '(apple juice bread) '(whiskey wine beer juice))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectall (cdr l-set)))))))

;(intersectall '((apple juice bread) (whiskey wine beer juice) (apple cider juice)))


(define a-pair?
  (lambda (x)
    (and
     (pair? x) (pair? (cdr x)) (null? (cdr (cdr x))))))

;(a-pair? '(1))
;(a-pair? '(1 42))
;(a-pair? '(1 42 43))


(define first
  (lambda (p)
    (car p)))


(define second
  (lambda (p)
    (car (cdr p))))


(define  build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define third
  (lambda (x)
    (car (cdr (cdr x)))))


(define rel?
  (lambda (l)
    (cond
      ((null? l) #t)
      (else (and (a-pair? (car l)) (rel? (cdr l)))))))

;(rel? '((1 2) (4 2)))


(define fun?
  (lambda (l)
    (set? (firsts l))))


;(fun? '((1 2) (4 2)))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (reverse (first rel)) (revrel (cdr rel)))))))

;(revrel '((1 2) (4 2)))


(define one-to-one?
  (lambda (rel)
    (fun? (revrel rel))))

(one-to-one? '((1 2) (4 3)))


(define entry1 '((appetizer entree beverage)
                 (pate boeuf vin)))

(define entry2 '(((beverage dessert)
                  ((food is) (number one with us)))))

(define entry3 '((entree dessert)
                 (spaghetti spumoni)))


(define look-up-in-entry
  (lambda (name entry entry-f)
    (look-up-in-entry-helper
     name
     (first entry)
     (second entry)
     entry-f)))
             
(define look-up-in-entry-helper
  (lambda (name keys values entry-f)
    (cond
      ((null? keys) (entry-f name))
      ((eq? (first keys) name) (first values))
      (else (look-up-in-entry-helper
             name
             (cdr keys)
             (cdr values)
             entry-f)))))
      
(define new-entry build)


;(look-up-in-entry 'appetizer entry1 identity)


(define extend-table cons)


(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (look-up-in-entry
             name
             (first table)
             (lambda (unusedName)
               (lookup-in-table
                name
                (cdr table)
                table-f)
               ))))))


;(lookup-in-table 'entree (cons entry1 (build entry2 entry3)) identity)

