; write a scheme function that returns the reverse of its
; simple list parameter

(define (reverse-list lst)
    (define rev-lst `())
    (define (reverse-list-closure lst rev-lst)
         (if (null? lst) rev-lst
         (reverse-list-closure (cdr lst) (cons (car lst) rev-lst))))
    (reverse-list-closure lst rev-lst))
