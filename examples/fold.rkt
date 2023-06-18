#lang racket

(define (foldl proc init ls)
    (if (null? ls)
        init
        (foldl proc (proc (car ls) init) (cdr ls))))