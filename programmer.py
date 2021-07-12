"""Program writer

This module writes programs for testing. All the programs do is access a
bunch of memory locations.

Maintainer: agua@ciencias.unam.mx
"""

import random
import sys

def lots_of_memory(n):
    """Writes a program that acceses n memory locations.
    
    Parameters
    ----------
    n : int
        Number of memory locations that the program should access.

    Returns
    -------
    s : string
        A string with the program that acceses n memory locations.
    """
    s = ""
    paren = 0
    for i in range(n):
        if i != n-1:
            s += "(x[" + str(i) + "] := 40;\n"
            paren += 1
        else:
            s += "x[" + str(i) + "] := 40"
    for i in range(paren):
        s += ")"
    return s

def lots_of_accesses(n):
    """Writes a program that uses only 5 memory locations but does n
       random accesses to it.
    
    Parameters
    ----------
    n : int
        Number of times the program should access memory locations.

    Returns
    -------
    s : string
        A string with the program that acceses n memory locations.
    """
    s = ""
    paren = 0
    for i in range(n):
        j = random.randrange(5)
        if i != n-1:
            s += "(x[" + str(j) + "] := 20;\n"
            paren += 1
        else:
            s += "x[" + str(j) + "] := 20"
    for i in range(paren):
        s += ")"
    return s

def write_program(filename, size, ptype):
    """Writes a program to a filename.
    
    Parameters
    ----------
    filename : string
        Name of the file where we will write the program (without file 
        extension).
    size : int
        Number of registers the written program will access. 
    ptype : function
        Function indicating what type of program will be written.
    """
    filename += ".imp"
    f = open(filename, "w")
    f.write(ptype(int(size)))
    f.close()
    
# Write program to filename
if len(sys.argv) < 4:
    raise ValueError("Use: python programmer.py <#registers> <program_name> memory/accesses")
else:
    writing_f = None
    if sys.argv[3] == "memory":
        writing_f = lots_of_memory
    elif sys.argv[3] == "accesses":
        writing_f = lots_of_accesses
    else:
        raise ValueError("Use: python programmer.py <#registers> <program_name> memory/accesses")
    write_program(sys.argv[2], sys.argv[1], writing_f)
