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
    (define (func-iter arg curr last1 last2 last3)
        (if (> arg n)
            curr
            (func-iter (+ arg 1) (+ last1 (* 2 last2) (* 3 last3)) curr last1 last2)
        )
    )

    (if (< n 3)
        n
        (func-iter 3 4 2 1 0)
    )
)
