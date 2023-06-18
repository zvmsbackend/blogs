#lang racket

(define (sum lower upper)
    (do ([acc 0 (+ acc lower)]
        [lower lower (add1 lower)])
        ((= lower upper) acc)))