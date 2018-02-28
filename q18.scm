; Write a scheme function that takes a simple list as its parameter
; and returns a list of all permutations of the given list

(define (permute-list lst)
    (cond 
        ((null? lst) `())
        ((null? (cdr lst)) (list lst))
        (else 
            (let break-up-list ((building-lst `()) (head (car lst)) (tail (cdr lst)))
                 (append (map (lambda (x) (cons head x)) (permute-list (append building-lst tail)))
                         (if (null? tail) `()
                             (break-up-list (cons head building-lst) (car tail) (cdr tail)))
                 )
            )
        )
    )          
)
   
    
