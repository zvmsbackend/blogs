#lang racket

(define-syntax-rule (when test body ...)
    (if test
        (begin
            body ...)
        (void)))