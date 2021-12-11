#!/usr/bin/python3

from numpy import genfromtxt, median, mean, vectorize
from sys import argv

data = genfromtxt(argv[1], delimiter=',').astype(int)
best = int(median(data))
print(sum(abs(data - best)))

def dist(a):
    def f(b):
        x = abs(a-b)
        return (x * (x+1)) // 2
    return f

print(min([sum(vectorize(dist(v))(data)) for v in range(min(data),max(data)+1)]))
