; Writing vector graphics scripts in autocad.
; Easier to prove/verify code, functional langs based on math.
; Easier to apply mathematics to programming.
; Чисти функции:
;   x1 = x2 => f(x1) = f(x2)
; => easier parallelisation, memoisation, no race conditions.
; окръжение = памет, state machine memory
; Without side effects, you can't know the program works,
; nothing to perceive.
; => make I/O stateful.
; Easier to work with sets. Used in databases and big data.
;     Sum x of I, x < 1000, I = {2*n, n of N}
; f(3, 5, 7) -> (f 3 5 7), f is a procedure
; Основна схема за оценяване
; (f x y z) -> x, y, z -> f(x, y, z)
; (if (< x 0) (- x) x)
; - -> operator-; (-) -> apply - to empty set
; (define <what> <how>)
(define (fact n)
        (if (= n 0) 
            1 
            (* n (fact (- n 1)))
        )        
)
; Recursion needs: bottom, self-reference, step.
; Fibbonacci: Fib(n) = Fib(n - 1) + Fib(n - 2), n > 2. Else 1.
; First try:
(define (fib_exp n)
    (if (< n 2)
        1
        (+ (fib_exp (- n 1)) (fib_exp (- n 2)))
    )
)
; Slow, cannot memoize from one branch to another.
; fib(n) depends on the last two computations, we need fib(n, last, pre-last).
; !! Sliding window algorithm. We forget what we used, only need some elements.
; We slide the window to keep our current elements.
; Fib(n) = 1, n < 2
;          F`(n, 2, 1, 1), F1 = F`(x-1), F2 = F`(x-2), F` = F1 + F2, x = n

(define (fib_lin n)
    (define (fib_lin_rec goal curr last prelast)
        (if (= goal curr)
            (+ last prelast)
            (fib_lin_rec goal (+ curr 1) (+ last prelast) last) 
        )
    )

    (if (< n 2)
        1
        (fib_lin_rec n 2 1 1)
    )
)
