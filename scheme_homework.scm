; write a scheme function that returns the reverse of its simple list parameter
(define (reverse-list lst)
    (define rev-lst `())
    (define (reverse-list-helper lst rev-lst)
        (if (null? lst)
            rev-lst
            (reverse-list-helper (cdr lst) (cons (car lst) rev-lst))))
    (reverse-list-helper lst rev-lst))

; write a scheme function that returns the union of two simple list parameters that represent sets
(define (union list1 list2)
    (if (null? list2) 
        list1
        (if (member (car list2) list1)
            (union list1 (cdr list2))
            (union (cons (car list2) list1) (cdr list2)))))

; write a scheme function that takes a simple list of numbers as its parameter and returns a list
; identical to the parameter list except with the numbers in ascending order
(define (insert x lst)
    (if (null? lst)
        (list x)
        (if (<= x (car lst))
            (cons x lst)
            (cons (car lst) (insert x (cdr lst))))))

(define (sort-asc lst)
    (if (null? lst)
        `()
        (insert (car lst) (sort-asc (cdr lst)))))

; write a scheme function that takes a simple list of numbers as its parameter and returns
; the largest and smallest numbers in the list.
(define (max-min lst)
    (define (max-min-helper lst max-num min-num)
        (if (null? lst)
            (list max-num min-num)
            (if (< (car lst) min-num)
                (max-min-helper (cdr lst) max-num (car lst))
                (if (> (car lst) max-num)
                    (max-min-helper (cdr lst) (car lst) min-num)
                    (max-min-helper (cdr lst) max-num min-num)))))
    (if (null? lst)
        `()
        (max-min-helper (cdr lst) (car lst) (car lst))))

; write a scheme function that takes a simple list as its parameter and 
; returns a list of all permutations of the given list
(define (permute-list lst)
    (if (null? lst)
        `()
        (if (null? (cdr lst))
            (list lst)
            (let break-up-list ((final-lst `()) (head (car lst)) (tail (cdr lst)))
                 (append (map (lambda (x) (cons head x)) (permute-list (append final-lst tail)))
                         (if (null? tail)
                             `()
                             (break-up-list (cons head final-lst) (car tail) (cdr tail))))))))
