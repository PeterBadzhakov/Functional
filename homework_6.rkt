(define (map f l)
    (if (null? l)
        `()
        (cons (f (car l)) (map f (cdr l)))
    )
)

(define (filter p? l)
    (if (null? l)
        `()
        (if (p? (car l))
            (cons (car l) (filter p? (cdr l)))
            (filter p? (cdr l))
        )
    )
)

(define (fold null op l)
    (if (null? l)
        null
        (op (car l) (fold null op (cdr l)))
    )
)

(define (foldl null op l)
    (define (help res l)
        (if (null? l)
            res
            (help (op res (car l)) (cdr l))
        )
    )

    (help null l)
)

(define (append l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (append (cdr l1) l2))
    )
)

(define (reverse l)
    (define (help curr res)
        (if (null? curr)
            res
            (help (cdr curr) (cons (car curr) res))
        )
    )

    (help l `())
)

(define (map-deep f l)
    (cond
        ((null? l) `())
        ((list? (car l)) (cons (map-deep f (car l)) (map-deep f (cdr l))))
        (else (cons (f (car l)) (map-deep f (cdr l))))
    )
)

(define (fold-deep f null l)
    (cond
        ((null? l) null)
        ((list? (car l)) (f (fold-deep f null (car l)) (fold-deep f null (cdr l))))
        (else (f (car l) (fold-deep f null (cdr l))))
    )
)

(define (filter-deep p? l)
    (cond
        ((null? l) `())
        ((list? (car l)) (cons (filter-deep p? (car l)) (filter-deep p? (cdr l))))
        (else 
            (if (p? (car l))
                (cons (car l) (filter-deep p? (cdr l)))
                (filter-deep p? (cdr l))
            )
        )
    )
)

(define (max-depth l)
    (define (max lhs rhs)
        (if (<= lhs rhs)
            rhs
            lhs
        )
    )
    (define (help l curr)
        (cond
            ((null? l) curr)
            ((list? (car l)) (max (help (car l) (+ curr 1)) (help (cdr l) curr)))
            (else (help (cdr l) curr))
        )
    )

    (help l 1)
)
