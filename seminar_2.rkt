; Функциите също са пълноценни променливи:
; могат да се ползват като аргументи и връщани стойности.
(define (fib n) ; Ако n < 1?
    (define (fib-iter cur fib1 fib2)
        (if (= n cur)
            fib1
            (fib_iter (+ cur 1) (+ fib1 fib2) fib1)
        )
    )
    (fib_iter 1 1 1)
)
; let по-добре за стойности, а define за функции.

(define (prime? n)
    (cond 
        ((< n 2) #f)
        ((= 2 2) #t)
        ((hasDiv? n) #f)
        (else #t)
    )
)

(define (hasDiv? n)
    (define (hasDivH x)
        (if (> (* x x) n)
            #f
            (if (= (modulo n x) 0)
                #t
                (hasDivH? (+ x 2))
            )
        )
    )
    (if (even? n)
        #t
        (hasDivH? 3)
    )
)

; o o o o o -> [] -> *` *' *" *^ -> [] -> *` *` *` -> [] -> @
;              ^~~map               ^~~filter         ^~~sum
; достатъчно за всичко.

;            0   +     stream +1   <n
(define (acc val oper  cur    next con)
    (if (con cur)
        (acc (oper val cur) oper (next cur) next cond)
        val
    )   
)

; ако се повтарят аргументи, ги define-ваме
(define (acc nul oper first next con)
    (define accH val cur)
    (if (con cur)
        (accH (oper val cur) (next cur))
        val
    )
    (accH nul first)
)

(define (sumPrimes n)
    (define (next n) (+ 2 n))
    (define (test x) (<= x n))
    (define (oper val x) (if (prime? x) (+ val x) val))
    (acc 2 oper 3 next test)
)

(define (hasDiv? n)
    (define (cntDiv n)
        (define (oper1 val x)
            (if (= 0 (modulo n x))
                (+ 1 val)
                val
            )
        )
        (define (inc x) (+ x 2))
        (define (con x) (<= (* x x) n))
        (acc 0 oper1 3 inc con)
    )
    (and (odd? n) (= 0 (cntDiv n)))
)

(define (prime? n)
;...
)

; Perfect number n: sum of divisors = n
(define (isPerf? n)
    (= n (acc 1 op 2 next cond?))
)
(define (sumPerfectTo n)
    (define (next x) (+ x 1))
    (define (cond? x) (<= x n))
    (define (oper val x) (if (isPerf? x) 
                                (+ val x) 
                                val))
    (acc 0 oper 1 next cond?)
)

; HOMEWORK
(derive f x)
(integrate f a b)





