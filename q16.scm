(define (insert x lst)
    (if (null? lst)
        (list x)
        (if (<= x (car lst))
              (cons x lst)
              (cons (car lst) (insert x (cdr lst))))))


(define (sort-asc lst)
    (if (null? lst)
                `()
                 (insert (car lst)
                 (sort-asc (cdr lst)))))
