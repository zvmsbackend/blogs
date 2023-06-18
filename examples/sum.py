#lang python

def sum(lower, upper):
    acc = 0
    while lower < upper:
        acc += lower
        lower += 1
    return acc