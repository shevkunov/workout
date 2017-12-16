from knapsack_test import *


np.random.seed(17)  # для воспроизводимости

### тест, проверяющий работоспособность псевдополиномиального алгоритма
# knapsack_stress_test(algo=knapsack_pseudopolynomial_nc)



### тест для полиномиального приближения (на целых числах)
# (точность такова, что полиномиальное решение
# на этом тесте всегда даёт оптимальный ответ)
polyester = lambda W, weights, costs: (
    knapsack_polynomial_estimation(W, weights, costs, eps=1e-2)
)
# knapsack_stress_test(algo=polyester)



### тест для полиномиального приближения (на целых числах)
# с этой точностью ответ не всегда оптимальный
# [этот тест проверяет на точное равенство оптимальному ответу
# и должен завершиться не успешно]
polyester = lambda W, weights, costs: (
    knapsack_polynomial_estimation(W, weights, costs, eps=0.3)
)
# knapsack_stress_test(algo=polyester)



### тест для полиномиального приближения (на вещественных числах)
# этот тест проверяет,что ответ не более чем в (1- eps) раз хуже
# knapsack_stress_test(algo=knapsack_polynomial_estimation,
#                      float_numbers=True, eps=0.3)



### тест для полиномиального приближения (на вещественных числах)
# уменьшим диапазон генерируемых стоимостей до (0, 5], чтобы
# не оптимальные ответы встречались чаще
#knapsack_stress_test(n_range=[10], maxcost=5,
#                     algo=knapsack_polynomial_estimation,
#                     float_numbers=True, eps=0.3)



### Подсчёт числа тестов, в которых ответ не оптимален, и средней точности
### на вещественных весах с маленьким тестом (100 тестов по 15 элементов,
# размер рюкзака - 15, стоимости в (0, 1])
"""
log = []
knapsack_stress_test(n_range=[15], w_range=[15], i_count=100, maxcost=1,
                     algo=knapsack_polynomial_estimation,
                     float_numbers=True, eps=0.1, log=log, print_log=False)

print("Test = ", len(log))
np_log = []
for i in range(len(log)):
    np_log.append([log[i][-4], log[i][-2]])
np_log = np.array(np_log)
print("Total different = ", (np.abs(np_log[:, 0] - np_log[:, 1]) > 1e-9).sum())
np_non_zero = np_log[:, 1] != 0
mean_accuracy = ((np_log[np_non_zero, 0] / np_log[np_non_zero, 1]).sum() + len(
    log) - np_non_zero.sum()) / len(log)
print("Mean accuracy = ", mean_accuracy)
"""
# Test =  100
# Total different =  2
# Mean accuracy =  0.999979750757



### Подсчёт числа тестов, в которых ответ не оптимален, и средней точности
### на целых весах со средним тестом (50 тестов по 50 элементов,
# , размер рюкзака - 50, стоимости в [1, 10], eps=0.1)
# [ответ в тесте среднего размера не может быть найден с помощью полного
# перебора и будет искаться псевдополиномиальным алгоритмом, требующим
# целых весов]
"""
log = []
knapsack_stress_test(n_range=[50], w_range=[50], i_count=50, maxcost=10,
                     algo=knapsack_polynomial_estimation,
                     checker=knapsack_pseudopolynomial_nc,
                     float_numbers=False, eps=0.1, log=log, print_log=False)

print("Test = ", len(log))
np_log = []
for i in range(len(log)):
    np_log.append([log[i][-4], log[i][-2]])
np_log = np.array(np_log)
print("Total different = ", (np.abs(np_log[:, 0] - np_log[:, 1]) > 1e-9).sum())
np_non_zero = np_log[:, 1] != 0
mean_accuracy = ((np_log[np_non_zero, 0] / np_log[np_non_zero, 1]).sum() + len(
    log) - np_non_zero.sum()) / len(log)
print("Mean accuracy = ", mean_accuracy)
"""
# Test =  50
# Total different =  0
# Mean accuracy =  1.0
# к чему бы это :)



### Тест большими по модулю, но различными числами.
"""
log = []
knapsack_stress_test(n_range=[15], w_range=[15], i_count=30,
                     maxcost=10000,
                     algo=knapsack_polynomial_estimation,
                     float_numbers=False, eps=0.1, log=log, print_log=False)

print("Test = ", len(log))
np_log = []
for i in range(len(log)):
    np_log.append([log[i][-4], log[i][-2]])
np_log = np.array(np_log)
print("Total different = ", (np.abs(np_log[:, 0] - np_log[:, 1]) > 1e-9).sum())
np_non_zero = np_log[:, 1] != 0
mean_accuracy = ((np_log[np_non_zero, 0] / np_log[np_non_zero, 1]).sum() + len(
    log) - np_non_zero.sum()) / len(log)
print("Mean accuracy = ", mean_accuracy)
"""
# Test =  30
# Total different =  0
# Mean accuracy =  1.0
# На больших по модулю, но различных значениях алгоритм работает хорошо



### Тест большими по модулю, и близкими числами.
"""
log = []
knapsack_stress_test(n_range=[15], w_range=[15], i_count=30,
                     maxcost=10000, mincost=9995,
                     algo=knapsack_polynomial_estimation,
                     float_numbers=False, eps=0.1, log=log, print_log=False)

print("Test = ", len(log))
np_log = []
for i in range(len(log)):
    np_log.append([log[i][-4], log[i][-2]])
np_log = np.array(np_log)
print("Total different = ", (np.abs(np_log[:, 0] - np_log[:, 1]) > 1e-9).sum())
np_non_zero = np_log[:, 1] != 0
mean_accuracy = ((np_log[np_non_zero, 0] / np_log[np_non_zero, 1]).sum() + len(
    log) - np_non_zero.sum()) / len(log)
print("Mean accuracy = ", mean_accuracy)

"""
# Test =  30
# Total different =  21
# Mean accuracy =  0.999953226882
# На больших по модулю, но близких числах алгоритм даёт неоптимальный
# ответ, что очевидно - по сути своей алгоритм округляет
# несколько близких чисел в одно.





### Тест с двумя кластерами
log = []
knapsack_stress_test(n_range=[15], w_range=[15], i_count=30,
                     maxcost=[3005, 5005], mincost=[3000, 5000],
                     algo=knapsack_polynomial_estimation,
                     cluster_test=True, float_numbers=False,
                     eps=0.1, log=log, print_log=False)

print("Test = ", len(log))
np_log = []
for i in range(len(log)):
    np_log.append([log[i][-4], log[i][-2]])
np_log = np.array(np_log)
print("Total different = ", (np.abs(np_log[:, 0] - np_log[:, 1]) > 1e-9).sum())
np_non_zero = np_log[:, 1] != 0
mean_accuracy = ((np_log[np_non_zero, 0] / np_log[np_non_zero, 1]).sum() + len(
    log) - np_non_zero.sum()) / len(log)
print("Mean accuracy = ", mean_accuracy)

# Test =  30
# Total different =  3
# Mean accuracy =  0.999982242818
# При увеличении числа кластеров, но постоянном числе предметов число тестов,
# на которых оптимальный ответ отличается от ответа полиномиального приближения
# значительно уменьшилось (3 / 30 против 21/30 для одного кластера)
# точность при этом осталась на высоком уровне.





### примеры тестов, на которых выводится неточный ответ, подобранные руками
"""
costs = np.array([40, 39, 40])
weights = np.array([2, 2, 3])
w = 6
print(knapsack_polynomial_estimation(w, weights, costs, 0.8))
print(knapsack_pseudopolynomial_nc(w, weights, costs))
print(knapsack_brutal(w, weights, costs))

costs = np.array([4.25446, 4.25346, 4.25446])
weights = np.array([2, 2.1, 3])
w = 6
print(knapsack_polynomial_estimation(w, weights, costs, 0.8))
print(knapsack_brutal(w, weights, costs))
"""