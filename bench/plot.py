#!/usr/bin/env python
import numpy as np
import matplotlib.pyplot as plt
import matplotlib
# matplotlib.rcParams.update({'font.size': 22})


benchs = ['ack', 'cpstak', 'fib', 'sum', 'tak']
programs = ['racket', 'pycket', 'firstclass', 'firstclass-fenv']
labels = ['Racket', 'Pycket', 'In firstclass language', 'With captured environment']
colors = ['lightgreen', 'green', 'skyblue', 'blue', 'magenta', 'purple', 'k', 'lightcoral', 'maroon']

# data = {}
# stddev = {}
# for program in programs:
#     means = []
#     dev = []
#     for bench in benchs:
#         d = open('./test-data/'+program+'-'+bench+'.csv')
#         print 'program: ', program, 'bench: ', bench
#         dt = [int(t) for t in d if t.strip() != '']
#         d.close()
#         mean = np.mean(dt)
#         means.append(mean)
#         dv = np.std(dt)
#         dev.append(dv)
#     stddev[program]= dev
#     data[program] = means
# # data to plot
# n_groups = len(benchs)
 
# # create plot
# fig, ax = plt.subplots()
# index = np.arange(n_groups)
# bar_width = 0.2

# for (i, (program,label,color)) in enumerate(zip(programs, labels, colors)):
#     plt.bar(index + i*bar_width, data[program], bar_width,
#                  color=color,
#                  label=label, yerr=stddev[program], log=False)

# plt.xlabel('Benchmarks')
# plt.ylabel('Time')
# plt.title('Comparison with Pycket and Racket')
# plt.xticks(index + bar_width*2, benchs)
# plt.legend()
 
# plt.tight_layout()
# plt.show()

fib, ax = plt.subplots()
js_benchs = ['fib', 'ack', 'tak', 'sum']
js_programs = ['javascript', 'javascript-fenv', 'lua', 'lua-fenv', 'luajit', 'luajit-fenv', 'pycket', 'firstclass', 'firstclass-fenv']
js_programs = ['javascript', 'lua', 'luajit', 'firstclass']
labels = ['Javascript', 'Javascript with captured environment',
          'lua', 'lua with captured environment',
          'luajit', 'luajit with captured environment',
          'Normal Program, Pycket',
          'Our implementation', 'Our implementation with capture environment']
means = {}
devs = {}
n_groups = len(js_benchs)
index = np.arange(n_groups)
bar_width = 0.1
for i, program in enumerate(js_programs):
    mean = []
    dev = []
    for p in js_benchs:
        print 'javascript: ', p
        d = open('./test-data/'+program+'-'+p+'.csv')
        data = [int(t) for t in d if t.strip() != '']
        print data
        mean.append(np.mean(data))
        dev.append(np.std(data))
    print 'program', program, mean, dev
    plt.bar(index+bar_width*i, mean, bar_width,
            color=colors[i], label=labels[i], yerr=dev, log=True)

plt.xlabel('Benchmarks')
plt.ylabel('Time')
lgd = plt.legend(loc='lower center', bbox_to_anchor=(0.5, -0.7))
#lgd = plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))

# plt.tight_layout()

plt.xticks(index+bar_width*3.5, js_benchs)
plt.ylim(ymin=1)
#plt.show()
plt.savefig('bench.pdf', format='pdf', bbox_extra_artists=(lgd,), bbox_inches='tight')
