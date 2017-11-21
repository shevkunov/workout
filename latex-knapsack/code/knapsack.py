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

    indexes = np.arange(C + 1)[A[n, :] < np.inf]
    if 0 == len(indexes):
        return 0, []  # ни один предмет не влезает
    k = indexes[-1]

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
    # assert (weights > 0).all()
    # assert (costs > 0).all()

    n = len(weights)

    best = (0, 0)

    for iteration in range(1, 2 ** n):
        i = iteration
        mass = 0
        cost = 0
        for j in range(n):
            if i % 2 == 1:
                mass += weights[j]
                cost += costs[j]
            i //= 2

        if (mass <= W) and (best[0] < cost):
            best = (cost, iteration)

    indexes = []
    i = best[1]
    for j in range(n):
        if i % 2 == 1:
            indexes.append(j)
        i //= 2

    return best[0], indexes


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

    costs_zipped = (np.copy(costs) / K).astype(int)
    nonzero_costs = costs_zipped != 0

    _, ans_on_nonzero = (
        knapsack_pseudopolynomial_nc(int(W),
                                     weights[nonzero_costs],
                                     costs_zipped[nonzero_costs])
    )

    ans = np.arange(n)[nonzero_costs][ans_on_nonzero]
    return costs[ans].sum(), ans

