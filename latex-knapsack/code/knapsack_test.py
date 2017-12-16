from knapsack import *


def knapsack_stress_test_nw(n, w, eps=None, maxcost=10, mincost=1,
                            algo=knapsack_pseudopolynomial_nc,
                            checker=knapsack_brutal,
                            cluster_test=False,
                            float_numbers=False,
                            log=None,
                            print_log=True):
    assert not (cluster_test and float_numbers)
    if cluster_test:
        costs = []
        length = len(mincost)
        for i in range(length):
            step_length = (
                n // length if i + 1 != length else n - len(costs)
            )
            costs += list(np.random.randint(low=mincost[i],
                                            high=maxcost[i],
                                            size=step_length))
        costs = np.array(costs)
        weights = np.random.randint(low=1, high=w + 2, size=n)
    elif not float_numbers:
        costs = np.random.randint(low=mincost, high=maxcost, size=n)
        weights = np.random.randint(low=1, high=w + 2, size=n)
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
                         i_count = 10,
                         maxcost=10,
                         mincost=1,
                         eps=None,
                         algo=knapsack_pseudopolynomial_nc,
                         checker=knapsack_brutal,
                         cluster_test=False,
                         float_numbers=False,
                         log=None,
                         print_log=True):
    for n in n_range:
        for w in w_range:
            for i in range(i_count):
                knapsack_stress_test_nw(n, w,
                                        maxcost=maxcost,
                                        mincost=mincost,
                                        eps=eps,
                                        algo=algo,
                                        checker=checker,
                                        cluster_test=cluster_test,
                                        float_numbers=float_numbers,
                                        log=log,
                                        print_log=print_log)
