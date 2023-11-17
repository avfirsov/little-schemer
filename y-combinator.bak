#lang scheme



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



;rember
;И ТАК МОЖНО ЗАМЕНИТЬ ЛЮБУЮ ФУНКЦИЮ
;ВООБЩЕ ЛЮБУЮ, С ЛЮБЫМ ЧИСЛОМ АРГУМЕНТОВ
(((lambda (mkFn)
    (mkFn mkFn mkFn))
  (lambda (mkFn mkFn1)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((equal? a (car lat)) ((mkFn mkFn1 mkFn1) a (cdr lat)))
        (else (cons (car lat) ((mkFn mkFn1 mkFn1) a (cdr lat))))))))
 1 '(1 2 3 4 5 6 7 8 9 1))