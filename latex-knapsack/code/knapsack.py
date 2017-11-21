import numpy as np


def knapsack_pseudopolynomial_nc(W, weights, costs):
    """ Псевдополиномиальное решение для задачи о рюкзаке.
    W - вместимость рюкзака
    weights - веса предметов
    costs - стоимости предметов
    weight и costs - numpy.ndarray или эквивалентные"""

    assert weights.shape == costs.shape
    # assert weights.dtype == int
    assert costs.dtype == int
    assert (weights > 0).all()
    assert (costs > 0).all()

    n = len(weights)
    C = costs.sum()

    A = np.ones((n + 1, C + 1), dtype=int) * np.inf
    A[0, 0] = 0
    for i in range(n):
        for s in range(C + 1):
            A[i + 1, s] = A[i, s]
            if ((s >= costs[i]) and
                    (A[i, s - costs[i]] + weights[i] <= min(W, A[i + 1, s]))):
                A[i + 1, s] = A[i, s - costs[i]] + weights[i]

    slice = np.arange(C + 1)[A[n, :] < np.inf]
    if (len(slice) == 0):
        return 0, []  # ни один предмет не влезает
    k = slice[-1]

    answer = []
    k_coordinate = k
    for i in range(n - 1, -1, -1):
        if A[i + 1, k_coordinate] == A[i, k_coordinate]:
            pass
        else:
            k_coordinate -= costs[i]
            answer.append(i)
    return k, answer


def knapsack_brutal(W, weights, costs):
    """ Решение задачи о рюкзаке полным перебором.
    W - вместимость рюкзака
    weights - веса предметов
    costs - стоимости предметов
    weight и costs - numpy.ndarray или эквивалентные"""

    assert weights.shape == costs.shape
    # assert weights.dtype == int
    # assert costs.dtype == int
    assert (weights > 0).all()
    assert (costs > 0).all()

    n = len(weights)

    best = (0, 0)

    for iteration in range(1, 2 ** n):
        i = iteration
        mass = 0
        cost = 0
        for j in range(n):
            if (i % 2 == 1):
                mass += weights[j]
                cost += costs[j]
            i //= 2

        if (mass <= W) and (best[0] < cost):
            best = (cost, iteration)

    indexes = []
    i = best[1]
    for j in range(n):
        if (i % 2 == 1):
            indexes.append(j)
        i //= 2

    return best[0], indexes


def knapsack_stress_test_nw(n, w, eps=None, maxcost=10,
                            algo=knapsack_pseudopolynomial_nc,
                            checker=knapsack_brutal,
                            float_numbers=False,
                            log=None,
                            print_log=True):
    if not float_numbers:
        costs = np.random.randint(low=0, high=maxcost, size=n) + 1
        weights = np.random.randint(low=0, high=w + 2, size=n) + 1
    else:
        costs = np.random.uniform(low=0, high=maxcost, size=n) + 1e-9
        weights = np.random.uniform(low=0, high=w + 2, size=n) + 1e-9

    if eps is None:
        c1, ans1 = algo(w, weights, costs)
    else:
        c1, ans1 = algo(w, weights, costs, eps=eps)

    c2, ans2 = checker(w, weights, costs)

    if print_log:
        print("===TEST===")
        print("W = ", w, ", eps = ", eps)
        print("weights = ", weights)
        print("costs = ", costs)
        print(c1, weights[ans1])
        print(c2, weights[ans2])

    assert costs[ans2].sum() == c2
    assert costs[ans1].sum() == c1

    if eps is None:
        assert c1 == c2
    else:
        # assert c1 <= c2 TODO ADD tolerance to comparsions
        assert (c2 == c1) or (c1 / c2 >= eps)

    if not log is None:
        log.append([w, eps, weights, costs, c1, ans1, c2, ans2])

def knapsack_stress_test(n_range=range(0, 10),
                         w_range=range(0, 10),
                         i_range=range(0, 10),
                         maxcost=10, eps=None,
                         algo=knapsack_pseudopolynomial_nc,
                         checker=knapsack_brutal,
                         float_numbers=False,
                         log=None,
                         print_log=True):
    for n in n_range:
        for w in w_range:
            for i in i_range:
                knapsack_stress_test_nw(n, w,
                                        maxcost=maxcost, eps=eps,
                                        algo=algo,
                                        checker=checker,
                                        float_numbers=float_numbers,
                                        log=log,
                                        print_log=print_log)


def knapsack_polynomial_estimation(W, weights, costs, eps):
    """Полиномиальное приближение задачи о рюкзаке.
    W - вместимость рюкзака
    weights - веса предметов
    costs - стоимости предметов
    eps - точность приближения (должна быть 0 < eps < 1) """

    assert W >= 0
    assert weights.shape == costs.shape
    assert 0 < eps < 1
    assert (weights > 0).all()
    assert (costs > 0).all()

    if not (weights <= W).any():
        return 0, []

    n = len(weights)
    P = costs[weights <= W].max()
    K = eps * P / n
    # print(K)
    costs_zipped = (np.copy(costs) / K).astype(int)
    nonzero_costs = costs_zipped != 0

    # print(costs_zipped)
    _, ans_on_nonzero = knapsack_pseudopolynomial_nc(int(W),
                                                     weights[nonzero_costs],
                                                     costs_zipped[
                                                         nonzero_costs])
    ans = np.arange(n)[nonzero_costs][ans_on_nonzero]
    return costs[ans].sum(), ans


# тест будет пройден
# knapsack_stress_test(algo=knapsack_pseudopolynomial_nc)



# тест для полиномиального приближения
# (точность такова, что почти всегда тест проходит
polyester = lambda W, weights, costs: (
    knapsack_polynomial_estimation(W, weights, costs, eps=1e-2)
)
# knapsack_stress_test(algo=polyester)



# тест для полиномиального приближения
# (так не проходит)
polyester = lambda W, weights, costs: (
    knapsack_polynomial_estimation(W, weights, costs, eps=0.3)
)
# knapsack_stress_test(algo=polyester)

# будем генерировать не целые веса и стоимости, а не целые
# knapsack_stress_test(algo=knapsack_polynomial_estimation,
#                      float_numbers=True, eps=0.5)

# очень мало тестов с отличиями.
# будем генерировать не целые веса и стоимости, а не целые
# knapsack_stress_test(n_range=[10], maxcost=5,
#                     algo=knapsack_polynomial_estimation,
#                     float_numbers=True, eps=0.3)

log = []
knapsack_stress_test(n_range=[15], maxcost=1,
                     algo=knapsack_polynomial_estimation,
                     float_numbers=True, eps=0.1, log=log, print_log=False)

print("Test = ", len(log))
np_log = []
for i in range(len(log)):
    np_log.append([log[i][-4], log[i][-2]])
np_log = np.array(np_log)
print("Total different = ", (np.abs(np_log[:, 0] - np_log[:, 1]) > 1e-9).sum())
np_non_zero = np_log[:, 1] != 0
mean_accuracy = ((np_log[np_non_zero, 0] / np_log[np_non_zero, 1]).sum() + len(log) - np_non_zero.sum()) / len(log)
print("Mean accuracy = ", mean_accuracy)
# примеры тестов, на которых выводится неточный ответ (да, точность никакая)
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
