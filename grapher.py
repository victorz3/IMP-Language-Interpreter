import matplotlib.pyplot as pp


def graph(yvals, title):
    # x axis values
    x = [100, 1000, 10000, 100000]
    pp.plot(x, yvals[0], label='listas')
    pp.plot(x, yvals[1], label='arreglos')
    pp.plot(x, yvals[2], label='árboles Patricia')    
    pp.xlabel('Número de direcciones de memoria')
    pp.ylabel('Tiempo en segundos') 
    pp.title(title)   
    pp.xscale("log")    
    pp.legend()   
    pp.show()


# y axis values for test 1
y0 = [0.5633, 0.5747, 0.7386, 1.9533]
y1 = [0.5631, 0.5652, 0.6873, 1.9184]
y2 = [0.5381, 0.5485, 0.695, 2.0976]
test1 = [y0, y1, y2]

# y axis values for test 2
y3 = [0.5624, 0.5503, 0.6965, 1.6068]
y4 = [0.6287, 0.6289, 0.8396, 2.2266]
y5 = [0.6439, 0.6605, 0.805, 4.6745]
test2 = [y3, y4, y5]

graph(test2, "Prueba 2")
