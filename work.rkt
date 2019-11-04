;TODO: Rename functions to use unit tests script!

(define (1+ n)
    (+ 1 n)
)
(define (my_for from to op state)
    (if (> from to)
        state
        (my_for (1+ from) to op (op from state))
    )
)

; Да се дефинира функцията fact-iter(n), която пресмята n! чрез итерация.

(define (fact-iter n)
    (my_for 1 n * 1)
)

; Да се дефинира функцията sum-iter(start, end), която пресмята 
; сумата на числата в интервала [start, end] чрез итерация.

(define (sum-iter start end)
    (my_for start end + 0)
)

; Да се дефинира функцията expt-iter(x, n), която пресмята xn чрез итерация.

(define (expt-iter x n)
    (case (signum n)
        ((1) (my_for 1 n (lambda (i j) (* x x)) 1))
        ((0) 1)
        ((-1) (/ 1 (expt-iter x (- n))))
    )
)

; Да се дефинира функцията count-digits(n), която намира броят на цифрите в 
; числото n. Реализирайте я с линейна рекурсия. Реализирайте я с линейна итерация.
(define (count-digits-rec n)
    (if (= n 0)
        1
        (+ 1 (count-digits-rec (/ n 10)))
    )
)
(define (count-digits-iter n)
    (define (helper n curr)
        (if (= n 0)
            curr
            (helper (/ n 10) (1+ curr))
        )
    )

    (helper n 1)
)

; Да се дефинира функцията sum-digits(n), която намира сумата на цифрите в 
; числото n. Реализирайте я с линейна рекурсия. Реализирайте я с линейна итерация.
(define (sum-digits-rec n)
    (if (= 0 n)
        0
        (+ (remainder n 10) (sum-digits-rec (/ n 10)))
    )
)
(define (sum-digits-iter n)
    (define (help n sum)
        (if (= n 0)
            sum
            (help (/ n 10) (+ (remainder n 10) sum))
        )
    )

    (sum-digits-iter n)
)

; Да се дефинира функцията reverse-digits(n), която връща число с цифрите 
; на числото n в обратен ред. Реализирайте я с линейна рекурсия. Реализирайте я 
; с линейна итерация.


; Да се дефинира функцията count-divisors(n), която брои колко делителя има 
; числото n. Реализирайте я с линейна рекурсия. Реализирайте я с линейна итерация.


; Да се дефинира функцията sum-divisors(n), която пресмята сумата на 
; делителите на числото n. Реализирайте я с линейна рекурсия. Реализирайте 
; я с линейна итерация.


; Да се дефинира предикат prime?(n), който проверява дали числото n е просто.


; Да се дефинира функцията fast-expt-iter(x, n), която пресмята xn чрез 
; бързо степенуване, но използвайки линейна итерация.
; Припомнете си свойството: Aко n е четно, то xn = (x(n/2))2.


