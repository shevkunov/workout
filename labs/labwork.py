import numpy as np
import matplotlib.pyplot as plt 
from math import sqrt

def sciPrintR(val, relErr, name=None):
    if name != None:
        print(name, val, "+-", val * relErr, "(", relErr * 100., "%)")
    else:
        print(val, "+-", val * relErr, "(", relErr * 100., "%)")
        
def sciPrintD(val, dErr, name=None):
    if name != None:
        print(name, val, "+-", dErr, "(", (dErr/val) * 100., "%)")
    else:
        print(val, "+-", dErr, "(", (dErr/val) * 100., "%)")
        
def prodErrorR(errors):
    errors = np.array(errors)
    return np.sqrt((errors**2).sum())

def prodErrorR_degs(errors):
    errors = np.array([(deg**2) * (error**2) for deg, error in errors])
    return np.sqrt(errors.sum())

def eval_mnk(x, y):
    assert len(x) == len(y)
    n = len(x)
    b = ((x*y).mean() - (x).mean() * (y).mean()) / ((x**2).mean() - (x).mean()**2)
    a = (y).mean() - b * (x).mean()
    sigma_b = 1./sqrt(n) * sqrt(((y**2).mean() - (y).mean()**2)/((x**2).mean() - (x).mean()**2) - b**2)
    sigma_a = sigma_b*sqrt((x**2).mean() - (x).mean()**2)
    return a, b, sigma_a, sigma_b

def plt_lab_figure(X_max, Y_max, X_min=0, Y_min=0, k_off_x=1.05, k_off_y=1.05):
    fig = plt.figure(figsize=(8, 16))
    ax = fig.add_subplot(111)
    
    x_minor_ticks = np.linspace(X_min / k_off_x, X_max * k_off_x + 0.0001, 125) # 104 
    x_major_ticks = np.array([x_minor_ticks[i] for i in range(0, x_minor_ticks.size, 20)])
    y_minor_ticks = np.linspace(Y_min / k_off_y, Y_max * k_off_y + 0.0001, 248) # 4822
    y_major_ticks = np.array([y_minor_ticks[i] for i in range(0, y_minor_ticks.size, 20)])


    ax.set_xticks(x_major_ticks)
    ax.set_xticks(x_minor_ticks, minor=True)
    ax.set_yticks(y_major_ticks)
    ax.set_yticks(y_minor_ticks, minor=True)
    ax.grid(which='minor', alpha=0.4, linestyle='-')
    ax.grid(which='major', alpha=0.7, linestyle='-')


    plt.xlim((X_min / k_off_x, X_max * k_off_x))
    plt.ylim((Y_min / k_off_y, Y_max * k_off_y))

