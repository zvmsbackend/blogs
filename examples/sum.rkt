#lang racket

(define (sum lower upper)
    (let loop ([acc 0] [lower lower])
        (if (= lower upper)
            acc
            (loop (+ acc lower) (add1 lower)))))