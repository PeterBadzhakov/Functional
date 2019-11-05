(define (1+ n)
    (+ 1 n)
)

(define (id x)
    x
)

(define (acc nv curr next func cond)
    (if (cond curr)
        (acc (func nv curr) (next curr) next func cond)
        nv
    )
)


; f(n) = n, ако n < 3 и
; f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3), ако n ≥ 3

(define (func n)
    (define (func-iter arg last1 last2 last3)
        ; TODO:
    )
)
