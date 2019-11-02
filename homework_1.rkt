; Да се дефинира функция add(x, y), която сумира x и y.
(define (add x y)
    (+ x y)
)

; Да се дефинира предикат even?(n), проверяващ дали n е четно число и предикат odd?(n), проверяващ дали n е нечетно число.
(define (even n)
    (= 0 (remainder n 2))
)
(define (odd n)
    (not (even n))
)

; Да се дефинира функцията signum(x), която връща -1, 0 или 1 в зависимост от това, дали x е отрицателно, нула или положително число.
(define (signum x)
    (cond
        ((> x 0) 1)
        ((= x 0) 0)
        ((< x 0) -1)
    )
)

; Да се дефинира функцията factorial(n), пресмятаща n!.
(define (fact n)
    (if (<= n 1)
        1
        ; n * fact(n - 1)
        (* n (fact (- n 1)))
    )
)

; Да се дефинира функция sum(start, end), която намира сумата на числата в интервала [start, end].
(define (sum_lin start end)
    (if (= start end)
        end
        ; start + sum_lin(start + 1, end)
        (+ start (sum_lin (+ start 1) end))
    )
)
(define (gauss_sum start end)
    ; Assumes start >= 0, start <= end
    ; (start + end) * (end - start + 1) / 2
    (if (> start end) 
        0
        (/ (* (+ start end) (+ end (- start) 1)) 2) 
    )
)
(define (sum_const start end)
    (cond
        ; 0 ... start ... end
        ((>= 0 (signum start)) (gauss_sum start end))
        ; start ... 0 ... end
        (+ (gauss_sum 0 end) (- (gauss_sum start 0)))
    )
)

; Да се дефинира функция expont(x, n), която пресмята xn.
(define (expont x n)
    (case (signum n)
        ; a ^ -b == (a ^ -1) ^ b
        ((-1) (expont (/ 1 x) (- n)))
        ((0) 1)
        ((1) (* x (expont x (- n 1))))
    )
)

; Да се дефинира функция fast-expont(x, n), която пресмята xn, използвайки следното свойство:
; Ако n е четно, то xn = (x(n/2))*(x(n/2)),
; иначе xn = x * x(n-1).
(define (fast-expont x n)
    (case (signum n)
        ; a ^ -b == (a ^ -1) ^ b
        ((-1) (fast-expont (/ 1 x) (- n)))
        ((0) 1)
        ((1) (if (even n)
                (* (fast-expont x (/ n 2)) (fast-expont x (/ n 2)))
                (* x (fast-expont x (- n 1)))
             )
        )
    )
)

