import numpy
import matplotlib.pyplot
import sys

col_num = int(sys.argv[1])
data = numpy.loadtxt('ampli.txt')

matplotlib.pyplot.plot(data[:, 0], data[:, col_num])
matplotlib.pyplot.show()
