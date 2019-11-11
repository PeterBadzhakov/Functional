; Zip, zip-with
; Zip - applies cons
; Zip-with - applies function
(define (zip-with l1 l2 f)
    (if (or (null? l1 ) (null? l2))
        `()
        (cons
            (f (car l1) (car l2))
            (zip-with (cdr l1) (cdr l2) f)
        )
    )
)

(define (zip l1 l2)
    (zip-with l1 l2 cons)
)

; n-ary zip?
(map f (apply map list square_matrix))

; map to matrix columns
(define (fun matrix f)
    (if (any? null? matrix) 
        `()
        (cons 
            (f (map car matrix)) ; head of each row = first column
            (fun (map cdr matrix) f) ; tail of each row = other columns
        )
    )
)

; Trees:
; (R `(L) `(R))
(define (create_tree x L R)
    (list x L R)
)
(define (get_root tree)
    (car tree)
)
(define (create_leaf x)
    (create_tree x `() `())
)

(define (find_bst? x tree)
    (cond
        ((null? tree) #f)
        ((= x (car tree)) #t) ; eqv? instead?
        ((< x (car tree)) (find_bst? x (cadr tree))) ; `(L)
        (else (find_bst? x (caddr tree))) ; `(R) ; cddr would be `((R))
    )
)

(define (add_bst? x tree)
    (cond
        ((null? tree) (list x `() `()))
        ((< (get_root tree) x) ; Add x to right subtree
            (create_tree 
                (get_root tree)
                (get-left tree)
                (add_bst x (get-right tree))
            )
        )
        (else 
            (create_tree  ; Add x to left subtree
                (get-root tree)
                (add_bst x (get_left tree))
                (get-right tree)
            )
        )
    )
)

