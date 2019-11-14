(define (above-diag m)
    (if (null? m)
        `()
        (cons (car m) (above-diag (map cdr (cdr m))))
    )
)


