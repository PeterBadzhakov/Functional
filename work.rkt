; While the condition is true over the current element,
; accumulate the current element into the current value via
; the operation, repeat over the next element.
(define (accumulate val oper cur next con)
    (if (con cur)
        (accumulate (oper val cur) oper (next cur) next cond)
        val
    )   
)

(define (1+ n)
    (+ 1 n)
)

; TODO: via accumulate, val is (curr . last)
(define (fib n)
    (define (fib-iter iter goal curr last)
        (if (>= iter goal)
            curr
            (fib-iter (1+ iter) goal (+ curr last) curr)
        )
    )

    (fib-iter 0 n 0 1)
)

(define (isDiv? what of)
    (if (= (remainder of what) 0)
        #t
        #f
    )
)

(define (prime? n)
    
)
