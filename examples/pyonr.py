#lang python

from "racket" import car, cdr, null_QUERY, list

def total(ls):
    def loop(acc, ls):
        if null_QUERY(ls):
            return acc
        return loop(acc + car(ls), cdr(ls))
    return loop(0, ls)

print total(list(1, 2, 3))