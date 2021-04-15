"""Canonical order string enumerator.

Program to enumerate strings in canonical order.
Canonical order of strings is ordering first by length and then by 
lexicographical order. 

Maintainer: agua@ciencias.unam.mx
"""

# The enum variable is used as a cache for previously computed results.
enum = [None, None]

def compute_strings(n):
    """Computes the canonical enumeration of strings up to size n and saves
       it in enum.

    Parameters
    ----------
    n : int
    Maximum size of computed strings minus one.
    """
    enum[0] = '0'
    enum[1] = '1'
    last_strings = enum[0:2]
    new_last = []
    for i in range(n):
        for s in last_strings:
            string = '0' + s
            enum.append(string)
            new_last.append(string)
        for s in last_strings:
            string = '1' + s
            enum.append(string)
            new_last.append(string)
        last_strings = new_last
        new_last = []

# Take size from user and compute the enumeration.
n = input("¿Maximum size of strings?\n")    
compute_strings(int(n)-1)
for e in range(len(enum)):
    print("n: " + str(e) + ", f(n): " + enum[e]) 
