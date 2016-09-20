import numpy as np
import matplotlib.pyplot as plt
 
benchs = ['ack', 'cpstak', 'fib', 'sum', 'tak']
programs = ['firstclass', 'pycket', 'racket', 'firstclass-fenv']

data = {}
for program in programs:
    means = []
    for bench in benchs:
        d = open('./test-data/'+program+'-'+bench+'.csv')
        print 'program: ', program, 'bench: ', bench
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

rects1 = plt.bar(index + 3*bar_width, data['firstclass-fenv'], bar_width,
                 color='c',
                 label='Our Language with Firstclass Environment')
plt.xlabel('Benchmarks')
plt.ylabel('Time')
plt.title('Comparison with Pycket and Racket')
plt.xticks(index + bar_width, benchs)
plt.legend()
 
plt.tight_layout()
plt.show()
