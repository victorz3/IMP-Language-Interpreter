import math
import matplotlib.pyplot as pp
import numpy as np

def graph(yvals, title):
    # x axis values
    x = [100, 1000, 10000, 100000]
    pp.plot(x, yvals[0], label='listas')
    pp.plot(x, yvals[1], label='arreglos')
    pp.plot(x, yvals[2], label='árboles Patricia')    
    pp.xlabel('Número de direcciones')
    pp.ylabel('Tiempo en segundos') 
    pp.title(title)   
    pp.xscale("log")    
    pp.legend()   
    pp.show()

def graphparseq(yvals, title):
    # x axis values
    x = [100000, 200000, 300000, 400000, 500000]
    pp.plot(x, yvals[0], label='Versión secuencial')
    pp.plot(x, yvals[1], label='Versión paralela')
    pp.xlabel('Número de programas')
    pp.ylabel('Memoria(MB)') 
    pp.title(title)   
    pp.legend()   
    pp.show()

def graph_program_size():
    # x axis values
    x = range(2, 2000)
    y1, y2, y3, y4, y5, y6, y7 = [], [], [], [], [], [], [] 
    for i in x:
        million = 1000000
        y1.append(program_size(million, i))
        y2.append(program_size(million*10, i))
        y3.append(program_size(million*100, i))
        y4.append(program_size(million*1000, i))
        y5.append(program_size(million*10000, i))
        y6.append(program_size(million*100000, i))
        y7.append(program_size(million*1000000, i))  
    # pp.plot(x, y1, label='1000000')
    # pp.plot(x, y2, label='10000000')
    # pp.plot(x, y3, label='100000000')
    # pp.plot(x, y4, label='1000000000')
    pp.plot(x, y5)#, label='10000000000')
    # pp.plot(x, y6, label='100000000000')
    # pp.plot(x, y7, label='1000000000000')
    pp.xlabel('Base')
    pp.ylabel('Tamaño')
    #pp.yscale('log')
    pp.title('Tamaño del programa 10000000000')   
    pp.legend()   
    pp.show()

def program_size(program, base):
    if base == 1:
        return program+1
    # Base 2 logarithm of base.
    base2Log = math.log2(base)
    # Digits to be used per character.
    digits = math.floor(base2Log) + 1
    if program == 0:
        return 2*digits
    baseBLog = math.floor(math.log(program, base)) 
    return (baseBLog + 2) * digits

#graph_program_size()
# numero = 10000000000
# for i in range(1,2000):
#     print("Tamaño de programa para la base %d: %d" % (i, program_size(numero, i)))

# y axis values for test 1
# y0 = [0.5148, 0.5868, 0.7688, 2.6102]
# y1 = [0.5964, 0.5944, 0.7728, 2.4218]
# y2 = [0.5718, 0.6128, 0.7778, 2.6452]
# test1 = [y0, y1, y2]

# # y axis values for test 2
# y3 = [0.623, 0.6544, 0.7368, 2.3104]
# y4 = [0.7012, 0.7404, 0.8234, 2.0668]
# y5 = [0.6232, 0.6424, 0.755, 2.0616]
# test2 = [y3, y4, y5]

#y axis for test 3
y6 = [242, 497, 815, 1053, 1443]
y7 = [1059, 2035, 2922, 3728, 4948]
test3 = [y6,y7]

# graph(test1, "Prueba 1")
graphparseq(test3, "Versión secuencial contra versión paralela")
