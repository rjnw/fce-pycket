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
# js_programs = ['javascript', 'javascript-fenv', 'lua', 'lua-fenv', 'luajit', 'luajit-fenv', 'pycket', 'firstclass', 'firstclass-fenv']
colors = ['green', 'blue', 'purple', 'lightcoral']
js_programs = ['javascript', 'lua', 'luajit', 'firstclass']
labels = ['Javascript', 'lua', 'luajit', 'Our Implementation']
means = {}
devs = {}
n_groups = len(js_benchs)
index = np.arange(n_groups)
bar_width = 0.1
for i, program in enumerate(js_programs):
    mean = []
    dev = []
    program_fenv = program+'-fenv'
    for p in js_benchs:
        print 'javascript: ', p
        d = open('./test-data/'+program+'-'+p+'.csv')
        denv = open('./test-data/'+program_fenv+'-'+p+'.csv')
        data = [int(t) for t in d if t.strip() != '']
        data_env = [int(t) for t in denv if t.strip() != '']
        print data
        mean.append(np.mean(data_env)/np.mean(data))
        #dev.append(np.std(data))
    print 'program', program, mean, dev
    plt.bar(index+bar_width*i, mean, bar_width,
            color=colors[i], label=labels[i], log=True)#, yerr=dev, log=True)

plt.xlabel('Benchmarks')
plt.ylabel('Slowdown relative to normal program')
lgd = plt.legend()
# lgd = plt.legend(loc='lower center', bbox_to_anchor=(0.5, -0.7))
#lgd = plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))

# plt.tight_layout()

plt.xticks(index+bar_width*3.5, js_benchs)
plt.ylim(ymin=1)
#plt.show()
plt.savefig('bench.pdf', format='pdf', bbox_extra_artists=(lgd,), bbox_inches='tight')



























fib, ax = plt.subplots()
js_benchs = ['fib', 'ack', 'tak', 'sum',
             'fenv-fib', 'fenv-ack', 'fenv-tak', 'fenv-sum']

js_programs = ['javascript', 'javascript-fenv', 'lua', 'lua-fenv', 'luajit', 'luajit-fenv', 'pycket', 'firstclass', 'firstclass-fenv']
js_programs_nf = ['javascript', 'lua', 'luajit', 'firstclass']
labels = ['Javascript', 'lua', 'luajit', 'Our Implementation']
colors = ['green', 'blue', 'purple', 'lightcoral']
means = {}
devs = {}
n_groups = len(js_benchs)
index = np.arange(n_groups)
bar_width = 0.1

for i, program in enumerate(js_programs_nf):
    mean = []
    dev = []
    program_fenv = program+'-fenv'
    for p in js_benchs:
        pf = p if 'fenv' not in p else p[5:]
        jsd = open('./test-data/javascript-'+pf+'.csv')
        jsnormal = np.mean([int(t) for t in jsd if t.strip() != ''])
        d = open('./test-data/'+program+'-'+p+'.csv')
        data = [int(t) for t in d if t.strip() != '']
        v = np.mean(data)/jsnormal
        print program, p, v
        mean.append(np.mean(data)/jsnormal)
    print 'program', program, mean, dev
    plt.bar(index+bar_width*i, mean, bar_width,
            color=colors[i], label=labels[i], log=True)#, yerr=dev, log=True)
pycket_mean = []
for bench in js_benchs[:4]:
    d = open('./test-data/pycket-'+bench+'.csv')
    jsd = open('./test-data/javascript-'+bench+'.csv')
    jsnormal = np.mean([int(t) for t in jsd if t.strip() != ''])
    data = [int(t) for t in d if t.strip() != '']
    v = np.mean(data)/jsnormal
    pycket_mean.append(np.mean(data)/jsnormal)
print 'pycketmean', pycket_mean
plt.bar(index+bar_width*4, pycket_mean+[0]*4, bar_width, color='black', label='Pycket', log=True)

plt.xlabel('Benchmarks')
plt.ylabel('Slowdown relative to JavaScript')
lgd = plt.legend()
#lgd = plt.legend(loc='lower center', bbox_to_anchor=(0.5, -0.3))
#lgd = plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))

# plt.tight_layout()

plt.xticks(index+bar_width*3, js_benchs)
plt.ylim(ymin=-10)
plt.axhline(y=1)
#plt.show()
plt.savefig('bench_norm.pdf', format='pdf', bbox_extra_artists=(lgd,), bbox_inches='tight')
