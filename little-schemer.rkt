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
     (null? l) '() (cons (car (car l)) (firsts (cdr l))))
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

(define sub1
  (lambda (x)
    (- x 1)))

;(sub1 42)

(define add1
  (lambda (x)
    (+ x 1)))



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


(define addtup
  (lambda (tup)
    (if
     (null? tup)
     0
     (➕ (car tup) (addtup (cdr tup))))
    ))

;(addtup '(1 2 3 4 5))


(define ×
  (lambda (x y)
    (if
     (zero? y)
     0
     (➕ x (× x (sub1 y))))
    ))


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


(define eternity
  (lambda (x)
    (eternity x)))

;(eternity 42)


;length0
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (eternity (cdr l))))))


;length1
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 ((lambda (l)
                   (cond
                     ((null? l) 0)
                     (else (add1 (eternity (cdr l)))))) (cdr l))))))


;length4 ORIGINAL
;цепочка вызовов, где каждый раз возвращается f(lat) = F(f(lat))
;после завершения самого глубокого вызова с (cdr lat), весь колстэк начнет схлопываться путем передачи туда (cdr lat)
; []                                                                        []                                 []                           [x2] <- (cdr lat)
;   []                                                                        []                                 [x1] <- (cdr (cdr lat))
;     []                                                                        [x0] <- (cdr (cdr (cdr lat))))                          
;       [] <- (cdr (cdr (cdr (cdr lat)))) ; последний вызов (с eternity), x0
;мы делаем cdr снаружи определяемой функции
;f1 = f0 cdr
;в каждый следующий уровень мы передаем аргументом cdr
;каждый новый уровень - (lat) => number и мы туда передаем cdr lat прошлого уровня
;финальный вызов - cdr^N lat
;каждая функция - "дай мне список и я дам тебе его длину но только если она не больше, чем заранее заложенно в мои способности"
;НЕВОЗМОЖНО МАСШТАБИРОВАТЬ БЕЗ ИЗМЕНЕНИЯ КОДА ФУНКЦИИ
;т.е. не существует такого уровня N, который можно сохранить в переменную, применить сам к себе и получить N+1
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 ((lambda (l)
                   (cond
                     ((null? l) 0)
                     (else (add1 (
                                  (lambda (l)
                                    (cond
                                      ((null? l) 0)
                                      (else (add1 ((lambda (l)
                                                     (cond
                                                       ((null? l) 0)
                                                       (else (add1 (eternity (cdr l)))))) (cdr l)))))) (cdr l)))))) (cdr l))))))







;абстрагируемый паттерн
;ДАЙ МНЕ LENGTH N и я тебе верну LENGTH N + 1
(lambda (length) 
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))


;length0
((lambda (length) 
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 eternity)


;length1
((lambda (length) 
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 ((lambda (length) 
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
  eternity))



;length3
;в каждый следующий уровень мы передаем аргументом f(length) и на самом последнем уровне передаем в качестве length eternity
; []                                  []                                 []                            [] <- f f f eternity ;f3(lat)
;   []                                  []                                 [] < f f eternity ;f2(lat)
;     []                                  [] <- f eternity ; f1(lat)
;       [] <- eternity ;вернет f0(lat)
;каждый новый уровень - (length) => (lat) => number
;финальный вызов - eternity
;т.е. по сравнению с оригиналом, мы абстрагировали фабрику уровней
;мы вынесли функцию "дай мне функцию вычисляющую длину для N и я тебе верну функцию длины для N+1"
;эта функция МАСШТАБИРУЕТСЯ
;т.е. мы можем сохранить основной паттерн в переменную x
;тогда length0 = x eternity
;length1 = x x eternity
;и так далее
;т.е. чтобы получить length N нам нужно применить вызвать x на eternity и потом зациклить вызов x N раз
((lambda (length) 
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 ((lambda (length) 
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
  ((lambda (length) 
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
   ((lambda (length) 
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    eternity))))



;обертка над ОСНОВНЫМ фрагментом
;length2
((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length eternity))))
 (lambda (length) 
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;length0 wo eternity
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (length) 
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l))))))))
 '())

;length1
(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length) 
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 ((mk-length eternity) (cdr l))))))))
 '(1))


;length
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;first working prototype
;проблема: больше не содержит ничего похожего на length => нельзя абстрагировать F(length)
(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length) 
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 ((mk-length mk-length) (cdr l))))))))
 '(1 2 3 4))




;НЕ РАБОТАЕТ, ХОТЯ МЫ ВСЕ ДЕЛАЛИ ПРАВИЛЬНО
'(((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     ((lambda (length)
        (lambda (l)
          (cond
            ((null? l) 0)
            (else (add1 (length (cdr l)))))))
      (mk-length mk-length)))))


;заменил в первом вызове mk-length
'(((lambda (mk-length)
     ((lambda (length)
        (lambda (l)
          (cond
            ((null? l) 0)
            (else (add1 (length (cdr l)))))))
      (mk-length mk-length)))
   (lambda (mk-length)
     ((lambda (length)
        (lambda (l)
          (cond
            ((null? l) 0)
            (else (add1 (length (cdr l)))))))
      (mk-length mk-length)))))


;раскрыл первый вызов
'(((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
   ((lambda (mk-length)
      ((lambda (length)
         (lambda (l)
           (cond
             ((null? l) 0)
             (else (add1 (length (cdr l)))))))
       (mk-length mk-length)))
    (lambda (mk-length)
      ((lambda (length)
         (lambda (l)
           (cond
             ((null? l) 0)
             (else (add1 (length (cdr l)))))))
       (mk-length mk-length))))))


;еще раскрыл mk-length
'(((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    ((lambda (mk-length)
       ((lambda (length)
          (lambda (l)
            (cond
              ((null? l) 0)
              (else (add1 (length (cdr l)))))))
        (mk-length mk-length)))
     (lambda (mk-length)
       ((lambda (length)
          (lambda (l)
            (cond
              ((null? l) 0)
              (else (add1 (length (cdr l)))))))
        (mk-length mk-length)))))))


;ДАЛЬШЕ ИДЕТ РАБОТА С РАБОЧИМ ПРОТОТИПОМ
;обернули (mk-length mk-length) в пустую лямбду
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length) 
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1
              ((lambda (x) ((mk-length mk-length) x))
               (cdr l))))))))


;вынесли length
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    (lambda (x) ((mk-length mk-length) x)))))



;вынесли кусок не зависящий от mk-length
(define Y
  (lambda (le)
    ((lambda (mk-length)
       (mk-length mk-length))
     (lambda (mk-length)
       (le (lambda (x) ((mk-length mk-length) x)))))))

(Y (lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l))))))))



((Y (lambda (only-even)
      (lambda (l)
        (cond
          ((null? l) '())
          ((even? (car l)) (cons (car l) (only-even (cdr l))))
          (else (only-even (cdr l)))))))
 '(1 2 3 4 5 6 7 8 9 10))

 




;1) (mk-length mk-length) //length
;2) mk-length === fn (mk-length)



;length n->n+1
(lambda (length)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))

;переход от верхней к нижней - самый сложный для вкуривания

;mk-length
(lambda (mk-length)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 ((mk-length mk-length) (cdr l)))))))

;length
(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 ((mk-length mk-length) (cdr l))))))))
 '(1 2 3 4 5 6 7 8 9))




;length с вынесенным mk-length mk-length
;НЕ РАБОТАЕТ
;ДАЕТ OUT OF MEMORY
;ПОТОМУ ЧТО РЕКУРСИЯ ИДЕТ НЕ ТУДА: РЕКУРСИЯ НАЧИНАЕТСЯ ДО ВЫПОЛНЕНИЯ ТЕРМИНАЛЬНОГО УСЛОВИЯ
;МОЖНО АБСТРАГИРОВАТЬ ЭТОТ ПРИМЕР
'((((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      ((lambda (length)
         (lambda (l)
           (cond
             ((null? l) 0)
             (else (add1 (length (cdr l)))))))
       (mk-length mk-length))))
   '(1 2 3 4 5 6 7 8 9)))



;А ВОТ ПОЧИНЕННЫЙ ВАРИАНТ, ПОЧИНКА ОБЕРТКОЙ
;Т.Е. КОЛБЭК ЧИНИТ РЕКУРСИЮ, ПОТОМУ ЧТО РЕКУРСИРОВАТЬ НАЧИНАЕМ НЕ ДО ТЕРМИНАЛЬНОГО УСЛОВИЯ,
;КАК РАНЬШЕ, А ПОСЛЕ, В МОМЕНТ ВЫЗОВА КОЛБЭКА
(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    ((lambda (length)
       (lambda (l)
         (cond
           ((null? l) 0)
           (else (add1 (length (cdr l)))))))
     (lambda (x)
       ((mk-length mk-length) x)))))
 '(1 2 3 4 5 6 7 8 9))





;length с колбэком вокруг (mk-length mk-length)
'((((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 ((lambda (x)
                         ((mk-length mk-length) x)) (cdr l))))))))
   '(1 2 3 4 5 6 7 8 9)))
