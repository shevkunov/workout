import numpy as np

def knapsack_pseudopolynomial_nc(W, weights, costs):
    """ Псевдополиномиальное решение для задачи о рюкзаке.
    W - вместимость рюкзака
    weights - веса предметов
    costs - стоимости предметов
    weight и costs - numpy.ndarray или эквивалентные"""

    assert weights.shape == costs.shape
    assert weights.dtype == int
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

    slice = np.arange(C+1)[A[n, :] < np.inf]
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
    assert weights.dtype == int
    assert costs.dtype == int
    assert (weights > 0).all()
    assert (costs > 0).all()

    n = len(weights)

    best = (0, [])

    for iteration in range(1, 2 ** n):
        i = iteration
        indexes = []
        for j in range(n):
            if (i % 2 == 1):
                indexes.append(j)
            i //= 2

        mass = weights[indexes].sum()
        cost = costs[indexes].sum()

        if (mass <= W) and (best[0] < cost):
            best = (cost, indexes)

    return best

def knapsack_integer_stress_test_nw(n, w, maxcost=10):
    costs = np.random.randint(low=0, high=maxcost, size=n) + 1
    weights = np.random.randint(low=0, high=w+2, size=n) + 1
    # costs = np.array([1, 10, 6, 5, 2])
    # weights = np.array([5, 7, 1, 6, 5])
    c2, ans2 = knapsack_brutal(w, weights, costs)
    c1, ans1 = knapsack_pseudopolynomial_nc(w, weights, costs)

    print("===TEST===")
    print("weights = ", weights)
    print("costs = ", costs)
    print(c2, weights[ans2])
    print(c1, weights[ans1])

    assert costs[ans2].sum() == c2
    assert costs[ans1].sum() == c1
    assert c1 == c2

def knapsack_integer_stress_test(max_n = 10, max_w = 10, max_i = 10):
    for n in range(max_n):
        for w in range(max_w):
            for i in range(max_i):
                knapsack_integer_stress_test_nw(n, w, maxcost = 20)

knapsack_integer_stress_test();

def
"""
costs = np.array([2, 3, 4])
weights = np.array([1, 2, 3])
w = 5
print(knapsack_pseudopolynomial_nc(w, weights, costs))
print(knapsack_brutal(w, weights, costs))
"""