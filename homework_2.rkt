(define (signum x)
    (cond
        ((> x 0) 1)
        ((= x 0) 0)
        ((< x 0) -1)
    )
)
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
        ((1) (my_for 1 n (lambda (i j) (* j x)) 1))
        ((0) 1)
        ((-1) (quotient 1 (expt-iter x (- n))))
    )
)

; Да се дефинира функцията count-digits(n), която намира броят на цифрите в 
; числото n. Реализирайте я с линейна рекурсия. Реализирайте я с линейна итерация.

;(define (count-digits-rec n)
;    (if (= n 0)
;        1
;        (+ 1 (count-digits-rec (quotient n 10)))
;    )
;)
(define (count-digits n)
    (define (helper n curr)
        (if (< n 10)
            curr
            (helper (quotient n 10) (1+ curr))
        )
    )

    (if (< n 0)
        (helper (- n) 1)
        (helper n 1)
    )
)

; Да се дефинира функцията sum-digits(n), която намира сумата на цифрите в 
; числото n. Реализирайте я с линейна рекурсия. Реализирайте я с линейна итерация.

;(define (sum-digits-rec n)
;    (if (= 0 n)
;        0
;        (+ (remainder n 10) (sum-digits-rec (quotient n 10)))
;    )
;)
(define (sum-digits n)
    (define (help n sum)
        (if (= n 0)
            sum
            (help (quotient n 10) (+ (remainder n 10) sum))
        )
    )

    (if (< n 0)
        (help (- n) 0)
        (help n 0)
    )
)

; Да се дефинира функцията reverse-digits(n), която връща число с цифрите 
; на числото n в обратен ред. Реализирайте я с линейна рекурсия. Реализирайте я 
; с линейна итерация.

(define (reverse-digits n)
    (define (help num state)
        (if (= num 0)
            state
            (help (quotient num 10) (+ (* state 10) (remainder num 10)))
        )
    )

    (help n 0)
)

; Да се дефинира функцията count-divisors(n), която брои колко делителя има 
; числото n. Реализирайте я с линейна рекурсия. Реализирайте я с линейна итерация.

(define (count-divisors n)
    (if (= n 0)
        1
        (if (< n 0)
            (my_for 
                1 
                (- n) 
                (lambda (i j) 
                    (if (= (remainder (- n) i) 0)
                        (1+ j)
                        j
                    )
                ) 
                0
            )
            (my_for 
                1 
                n 
                (lambda (i j) 
                    (if (= (remainder n i) 0)
                        (1+ j)
                        j
                    )
                ) 
                0
            )
        )
    )
)

; Да се дефинира функцията sum-divisors(n), която пресмята сумата на 
; делителите на числото n. Реализирайте я с линейна рекурсия. Реализирайте 
; я с линейна итерация.

(define (sum-divisors n)
    (if (= n 0)
        1
        (if (< n 0)
            (my_for
                1
                (- n)
                (lambda (i j)
                    (if (= (remainder (- n) i) 0)
                        (+ i j)
                        j
                    )
                )
                0
            )
            (my_for
                1
                n
                (lambda (i j)
                    (if (= (remainder n i) 0)
                        (+ i j)
                        j
                    )
                )
                0
            )
        )
    )
)


; Да се дефинира предикат prime?(n), който проверява дали числото n е просто.

(define (prime? n)
    (and (> n 1) (= (count-divisors n) 2))
)

; Да се дефинира функцията fast-expt-iter(x, n), която пресмята xn чрез 
; бързо степенуване, но използвайки линейна итерация.
; Припомнете си свойството: Aко n е четно, то xn = (x(n/2))2.

; Кешираме не само степента, а и основата, за да няма
; (* (helper ...) (helper ...)) при четни.
(define (fast-expt-iter x n)
    (define (square x) (* x x))
    (define (helper result base exponent)
        (cond 
            ((< exponent 0) (quotient 1 (helper result base (- exponent))))
            ((= exponent 0) result)
;           x^0  =              1
            ((= exponent 1) (* result base))
;           x^1  =      x *     1
            ((even? exponent)
                (helper result (square base) (quotient exponent 2))
;           x^10 =      x *     x^5 * x^5
            )
            (else
                (helper (* base result) (square base) (quotient (- exponent 1) 2))
;           x^11 =      x *     x^5 * x^5
            )
        )
    )

    (helper 1 x n)
)
