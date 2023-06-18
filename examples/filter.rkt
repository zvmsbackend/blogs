#lang racket

(define (filter proc ls)
    (do ([ls ls (cdr ls)]
        [acc null (if (proc (car ls)) (cons (car ls) acc) acc)])
        ((null? ls) (reverse acc))))