; Lists with arbitrary levels of depth can represent data structures
; like graphs, trees, and lists.
; (1 (2 3 (4 5)))
; 1
; -- 2
; -- 3
; -- 4, 5

; Lists with two levels of depth have two simultaneous recursions.
; Base: downwards is empty, right is empty.

; (1 (2 3) 4 5 ((6) (7 (8) 9 10)))
(define (flatten l)
    (cond 
        ((null? l) l)
        ; depth
        ((list? (car l)) (append (flatten (car l)) (flatten (cdr l))))
        ; breadth
        (else (cons (car l) (flatten (cdr l))))
    )
)
; depth -> breadth to keep original element order.
; Recursive process due to 17:append

; Max depth of graph.
(define (depth l)
    (define (depth-hlp l result)
        (cond 
            ((null? l) result)
            ((list? (car l)) (max 
                                    (depth-hlp (car l) (+1 res))
;                                                       ^~curr is list, so +1
                                    (depth-hlp (cdr l) res)       
                             ))
            (else (depth-hlp (cdr l) res))
        )
    )
    (depth-hlp l 1)
)

; Sum of graph elements
(define (sum l)
    (define (sum-hlp l result)
        (cond
            ((null? l) result)
            ((list? (car l)) (+ 
                                (sum-help (car l) res)
                                (sum-help (cdr l) res)
                             ))
            (else (sum-hlp (cdr l) (+ (car l) res)))
        )
    )

    (sum-hlp l 0)
)
