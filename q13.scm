; write a scheme function that returns the union of two
; simple list parameters that represent sets

(define (union list1 list2)
    (cond 
      ((null? list2) list1)
      ((member (car list2) list1) (union list1 (cdr list2)))
      (else (union (cons (car list2) list1) (cdr list2)))
    )
)
