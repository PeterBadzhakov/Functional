; List transformations:
; 	before: (1 (2 3) (4 (5 6) 7) 8)
;	after:  1 -> (1 2); 2 -> (2 3); 3 -> (3 4) ...
;			    ^~~(2 3) -> ((2 3) (3 4)) ...
;           .         .	
;	S.t.   /         / \
;	      x    =>  x    x+1

(define (transform l)
	(deep-map 
		(lambda (x)
			(list x (+ x 1)		
		)
		l	
	)
)

(define (transform l)
	(cond 
		((null? l) l)
		((list? (car l)) 
			(cons
				(transform (car l)) ; Go down.
				(transform (cdr l)) ; Go right.
			)	
		)
		(else
			; Atom for unit element.		
			(cons 
				(list (car l) (+ 1 (caar l)))
				(transform (cdr l))
			)			
		)	
	)
)


(define (clone x)
	(define (hlp x cnt res)
		(if (= cnt 0)
			res
			(hlp x (- cnt 1) (cons x res))			
		)	
	)

	(hlp x x `())
)


;Depth sum:
;	List of sums of each level.
;	(1 (2 3) (4 (5 6) 7) (8)) 
;	=> ((1.0) (2.1) (3.1) (4.1) (5.2) (6.2) (7.1) (8.1) (2.0)) 
;	=> (3 . ((2.0) (3.0) (4.0) (5.1) (6.1) (7.8) (8.0))	
;	=> (1 24 11)

; Flatten by levels
	(define (flat-lvl l cl)
		(cond 
			((null? l) l)
			((list? (car l)) (append (flat_lvl (car l) (+ cl 1)
						 (flat_lvl (cdr l) cl)))
			(else (cons (cons (car l) cl) (flat_lvl (cdr l) cl))			
		)
	)
	(define (sum-0 l)
		(foldl 
			(lambda (x y) (+ x y)) 
			0 
			(map car (filter (lambda (x) (= 0 (cdr x)) l))
		)	
	)
	(define (step l)
		(cons (sum-0 l) (filter-0 l))	
	)
	(define (filter-0 l)
		(map 
			(lambda (x) (cons (car x) (- (cdr x) 1))) 
			(filter (lambda (x) (< 0 (cdr x))) l)
		)
	)

	(define (sum-lvl l)
		(if (null? (cdr l))
			(list (car l))
			(cons (car l) (sum-lvl (step (cdr l))))
		)
	)

	(define (task-lvl l)
		(sum-lvl (step (flat_lvl l 0)))	
	)
