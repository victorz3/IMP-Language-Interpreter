import matplotlib.pyplot as pp

# x axis values
x = [100, 1000, 10000, 100000]

# y axis values
y00 = [0.5169, 0.5675, 0.7258, 2.0476]
y10 = [0.5623, 0.5779, 0.7313, 2.0263]
y20 = [0.5401, 0.5448, 0.6839, 2.0739]
y01 = [0.5633, 0.5747, 0.7386, 1.9533]
y11 = [0.5631, 0.5652, 0.6873, 1.9184]
y21 = [0.5381, 0.5485, 0.695, 2.0976]

pp.plot(x, y00, label='listas sin optimizar')
pp.plot(x, y10, label='arreglos sin optimizar')
pp.plot(x, y20, label='árboles Patricia sin optimizar')
pp.plot(x, y01, label='listas con optimizaciones')
pp.plot(x, y11, label='arreglos con optimizaciones')
pp.plot(x, y21, label='árboles Patricia con optimizaciones')

pp.xlabel('Número de direcciones de memoria')
pp.ylabel('Tiempo en segundos')

pp.xscale("log")

pp.legend()


pp.show()
