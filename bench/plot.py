import numpy as np
import matplotlib.pyplot as plt
 
benchs = ['ack', 'cpstak', 'fib', 'sum', 'tak']
programs = ['firstclass-fenv', 'firstclass', 'pycket', 'racket']
labels = ['Programs with captured environments', 'Normal programs in our language', 'Pycket', 'Racket']
colors = 'cbgy'#['c','b','g','y']
data = {}
for program in programs:
    means = []
    for bench in benchs:
        d = open('./test-data/'+program+'-'+bench+'.csv')
        print 'program: ', program, 'bench: ', bench

        mean = np.mean([int(t) for t in d if t.strip() != ''])
        means.append(mean)
        data[program] = means

# data to plot
n_groups = len(benchs)
 
# create plot
fig, ax = plt.subplots()
index = np.arange(n_groups)
bar_width = 0.2

for (i, (program,label,color)) in enumerate(zip(programs, labels, colors)):
    plt.bar(index + i*bar_width, data[program], bar_width,
                 color=color,
                 label=label)

plt.xlabel('Benchmarks')
plt.ylabel('Time')
plt.title('Comparison with Pycket and Racket')
plt.xticks(index + bar_width, benchs)
plt.legend()
 
plt.tight_layout()
# plt.show()

js_programs = ['fib', 'fib_fenv', 'fib_rfenv']
data = {}
n_groups = len(js_programs)
index = np.arange(n_groups)
bar_width = 0.3
for i,p in enumerate(js_programs):
    print i, p
    d = open('./test-data/javascript-'+p+'.csv')
    mean = [int(t) for t in d if t.strip() != '']
    data[p] = mean
plt.bar(index+bar_width*i, np.mean(mean), bar_width, color[i], p)
plt.show()
