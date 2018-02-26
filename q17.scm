; write a scheme function that takes a simple list of numbers
; as its parameter and returns the largest and smallest numbers
; in the list.

(define (max-min lst)
    (define (max-min-closure lst max-num min-num)
        (cond 
            ((null? lst) (list max-num min-num))
            ((< (car lst) min-num) (max-min-closure (cdr lst) max-num (car lst)))
            ((> (car lst) max-num) (max-min-closure (cdr lst) (car lst) min-num))
            (else (max-min-closure (cdr lst) max-num min-num))
         )
    )
    (if (null? lst)
        `()
        (max-min-closure (cdr lst) (car lst) (car lst)))
)
