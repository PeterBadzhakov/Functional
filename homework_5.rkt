;	В дефинициите на sum, product и accumulate от слайдовете при всяко рекурсивно 
;	извикване на съответната функция към аргумента a се прибавя 1. Напишете по-общ 
;	вариант на функциите sum, product и accumulate, като добавите нов параметър към 
;	съответната функция, който да пресмята следващата стойност на a. Пример:
(define (identity x) x)
(define (1+ x) (+ x 1))
(define (2+ x) (+ x 2))
;	(sum identity 1 2+ 5) ; 9
;	(product identity 1 2+ 5) ; 15
;	(accumulate + 0 identity 1 2+ 5) ; 9
;	(accumulate * 1 identity 1 2+ 5) ; 15
(define (sum from to next term)
    (if (> from to)
        0
        (+
            (term from)
            (sum (next from) to next term)
        )
    )
)
(define (prod from to next term)
    (if (> from to)
        1
        (*
            (term from)
            (prod (next from) to next term)
        )
    )
)
(define (accumulate func from to next term null)
    (if (> from to)
        null
        (func
            (term from)
            (accumulate func (next from) to next term null)
        )
    )
)

;	Дефинирайте accumulate чрез итерация.
(define (accumulate-i func from to next term null)
    (if (> from to)
        null
        (accumulate-i func (next from) to next term (func null (term from)))
    )
)

;	Напишете функция count(predicate, a, b), която връща броя на елементите в 
;	интервала [a, b], за които предикатът predicate е истина. Ако не сте я 
;	имплементирали чрез accumulate, имплементирайте я чрез accumulate.
(define (count pred a b)
    (define (bool_to_int x)
        (if (pred x)
            1
            0
        )
    )

    (accumulate-i + a b 1+ bool_to_int 0)
)

;	Напишете функция count-palindromes(a, b) , която преброява палиндромите в 
;	нтервала [a, b].
(define (palindrome? a)
    (define (reverse-digits n)
        (define (help num state)
            (if (= num 0)
                state
                (help (quotient num 10) (+ (* state 10) (remainder num 10)))
            )
    
        )
        
        (help n 0)
    )

    (or 
        (= a 0)
        (and 
            (= (remainder a 10) (remainder (reverse-digits a) 10)) 
            (palindrome? (quotient (reverse-digits (quotient a 10)) 10))
        )
    )
)
(define (count-palindromes a b)
    (count palindrome? a b)
)

;	Напишете предикат exists?(predicate, a, b), който проверява дали съществува 
;	цяло число в интервала [a, b], за което предикатът predicate е истина.
(define (exists? p a b)
    (or (> (count p a b) 0) #f)
)

;	Напишете предикат for-all?(predicate, a, b), който проверява дали за всяко 
;	цяло число в интервала [a, b] предикатът predicate е истина.
(define (for-all? p a b)
    (= (count p a b) (+ a (- b) 1))
)

;	Напишете функция double, която приема функция на един аргумент и връща функция, 
;	която прилага подадената функция два пъти. Например, ако имаме функцията inc, която добавя 1 към своя аргумент ((define (inc x) (+ x 1)), то (double inc) е функция, която добавя 2 към своя аргумент.



;	Нека f и g са функции на един аргумент. Композицията f ∘ g е функцията x ↦ f(g(x)). 
;	Напишете функция compose(f, g), която връща композицията f ∘ g. Пример:



;	(define (inc x) (+ x 1))
;	(define (square x) (* x x))



;	((compose square inc) 6) ; 49



;	Нека f е функция на един аргумент и n е цяло неотрицателно число. Дефинираме 
;	n-тото прилагане на функцията f да бъде функцията, дефинирана по следния начин:



;	f0(x) = x



;	fn(x) = f(fn-1(x))



;	Напишете функция repeated(f, n), която връща n-тото прилагане на f. Пример:



;	(define (square x) (* x x))



;	((repeated square 2) 5) ; 625



