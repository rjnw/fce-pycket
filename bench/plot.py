import numpy as np
import matplotlib.pyplot as plt
 
benchs = ['ack', 'cpstak', 'fibc', 'fib', 'sum', 'tak', 'ctak']
programs = ['firstclass', 'pycket', 'racket']

data = {}
for program in programs:
    means = []
    for bench in benchs:
        d = open('./test-data/'+program+'-'+bench+'.rkt.csv')
        mean = np.mean([int(t) for t in d])
        means.append(mean)
        data[program] = means

# data to plot
n_groups = len(benchs)
 
# create plot
fig, ax = plt.subplots()
index = np.arange(n_groups)
bar_width = 0.2

rects1 = plt.bar(index, data['firstclass'], bar_width,
                 color='b',
                 label='FirstClass Environment')
 
rects2 = plt.bar(index + bar_width, data['pycket'], bar_width,
                 color='g',
                 label='Pycket')
 
rects2 = plt.bar(index + bar_width + bar_width, data['racket'], bar_width,
                 color='r',
                 label='Racket')

plt.xlabel('Benchmarks')
plt.ylabel('Time')
plt.title('Comparison with Pycket and Racket')
plt.xticks(index + bar_width, benchs)
plt.legend()
 
plt.tight_layout()
plt.show()
