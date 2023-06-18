#lang racket

(define (reverse ls)
    (foldl cons null ls))