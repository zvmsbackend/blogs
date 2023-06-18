#lang racket

(define (map proc ls)
    (do ([ls ls (cdr ls)]
        [acc null (cons (proc (car ls) acc))])
        ((null? ls) (reverse acc))))