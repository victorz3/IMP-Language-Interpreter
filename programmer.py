import sys

def programa(n):
    cuenta_par = 0
    for i in range(n):
        if i != n-1:
            print("(x[" + str(i) + "] := 40;")
            cuenta_par += 1
        else:
            print("x[" + str(i) + "] := 40", end="")
    for i in range(cuenta_par):
        print(")", end="")


programa(int(sys.argv[1]))
