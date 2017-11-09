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
    
    x_minor_ticks = np.linspace(X_min / k_off_x, X_max * k_off_x, 125) # 104 
    x_major_ticks = np.array([x_minor_ticks[i] for i in range(0, x_minor_ticks.size, 20)])
    y_minor_ticks = np.linspace(Y_min / k_off_y, Y_max * k_off_y, 248) # 4822
    y_major_ticks = np.array([y_minor_ticks[i] for i in range(0, y_minor_ticks.size, 20)])


    ax.set_xticks(x_major_ticks)
    ax.set_xticks(x_minor_ticks, minor=True)
    ax.set_yticks(y_major_ticks)
    ax.set_yticks(y_minor_ticks, minor=True)
    ax.grid(which='minor', alpha=0.4, linestyle='-')
    ax.grid(which='major', alpha=0.7, linestyle='-')


    plt.xlim((X_min / k_off_x, X_max * k_off_x))
    plt.ylim((Y_min / k_off_y, Y_max * k_off_y))

def linPlot(x, y, xlabel="", ylabel="", title="", figsize=(14,7), fontsize=15, labplot=False):
    """Строит график измерений x,y и линейное приближение
    зависимости по МНК (y = bx + a).
    Возвращает: a, b, sigma_a, sigma_b"""
    if (not labplot):
        plt.figure(figsize=(14,7))
    else:
        plt_lab_figure(x.max(), y.max())
    a, b, sigma_a, sigma_b = eval_mnk(x, y)
    xs = np.array([x.min(), x.max()])
    plt.plot(xs, b*xs + a, label="Оценка МНК")
    plt.scatter(x, y, label="Измерения", color="red")
    plt.xlabel(xlabel, fontsize=fontsize)
    plt.ylabel(ylabel, fontsize=fontsize)
    plt.title(title, fontsize=fontsize)
    plt.grid()
    plt.legend()
    plt.show()
    return a, b, sigma_a, sigma_b

def plotIntervals(x, x_std, y, y_std, xlabel="Значения",
                  ylabel="Номер измерения", title="", fontsize=15):
    """Строит сравнительный график значений x[i] c y[i],
    где x_std[i] и y_std[i] - их стандартные отклонения"""
    assert len(x) == len(x_std)
    assert len(y) == len(y_std)
    assert len(x) == len(y)
    plt.figure(figsize=(10, len(x)))
    hstep = 1
    hsmall = 0.1
    for i in range(len(x)):
        plt.hlines((i + 1) * hstep - hsmall, x[i] - x_std[i], x[i] + x_std[i])
        plt.hlines((i + 1) * hstep + hsmall, y[i] - y_std[i], y[i] + y_std[i])
        plt.scatter([x[i], y[i]], [(i + 1) * hstep - hsmall, (i + 1)*hstep + hsmall], )
    plt.grid()
    plt.xlabel(xlabel, fontsize=fontsize)
    plt.ylabel(ylabel, fontsize=fontsize)
    plt.title(title, fontsize=fontsize)
    plt.show()

def bordering(V):
    shift = -int(np.log10(V)) + 2
    V_N = V * 10 **(shift)
    while (int(V_N + 0.5)  >= 100):
        V_N /= 10.
        shift -= 1
    while (int(V_N + 0.5) < 10):
        V_N *= 10.
        shift += 1
    return int(V_N + 0.5), shift

def sciRoundR(V, V_R, unit=""):
    """По числу V и его относительной погрешности V_R
    возвращает строку, в которой число округлено по
    правилам лобораторных работ"""
    V_E = V * V_R
    V_E_int, shift_e = bordering(V_E)
    if (V_E_int > 17):
        shift_e -= 1
    V_R_int, shift_r = bordering(V_R)
    # print(V_E)
    if (V_R_int > 17):
        shift_r -= 1
    V_int = int(V * 10 ** (shift_e) + 0.5)
    V_E_int = int(V_E * 10 ** (shift_e) + 0.5)    
    V_R_int = int(V_R * 10 ** (shift_r) + 0.5)
    
    zeros_add = 0
    if shift_e % 3 != 0:
        zeros_add = 3 - (shift_e % 3) 
    
    
    result = str(V_int * 10 **(zeros_add)) + " ± " + str(V_E_int *  10 **(zeros_add))
    if (shift_e + zeros_add != 0) or (unit != ""):
        if unit != "":
            if (shift_e + zeros_add!= 0):
                result += " [1e" + str(-shift_e-zeros_add) + " x " + unit + "]"
            else:
                
                result += " [" + unit + "]"
        else:
            result += " [1e" + str(-shift_e-zeros_add) + "]"
        
    return (
        
        result + " (" + str(V_R_int * 10 ** (-shift_r+2)) + "%)"
    )
    
