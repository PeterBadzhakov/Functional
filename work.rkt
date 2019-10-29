; Sumdivisors
(define (accumulate op nv a b term next)
    (if (> a b) 
        nv
        (op (term a) (accumulate op nv (next a) b term next))
    )
)
