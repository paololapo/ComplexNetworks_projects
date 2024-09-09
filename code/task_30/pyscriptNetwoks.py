import numpy as np
import networkx as nx
import matplotlib.pyplot as plt
import pandas as pd
import multiprocessing as mp
import itertools
from time import time

from dynamics import *

## Parallel Processing ##
N = int(1e3)
networks = [
    nx.barabasi_albert_graph(N, 5), 
    nx.barabasi_albert_graph(N, 1),
    nx.erdos_renyi_graph(N, 0.002),
    nx.erdos_renyi_graph(N, 0.007),
    nx.erdos_renyi_graph(N, 0.012),
]

labels = [
    "BA_5", 
    "BA_1", 
    "ER_0-002", 
    "ER_0-007", 
    "ER_0-012", 
]


F_list = [2, 10]
q_list = [1, 2, 4, 6, 8, 10, 100, 200]
rep_mc = 4
max_iterations = 5e7

log_scale = np.unique(np.logspace(0, np.log10(max_iterations), 96, base=10, endpoint=True, dtype=int))


for g, label in zip(networks, labels):
    combinations = list(itertools.product([g], F_list, q_list, [rep_mc], [max_iterations], [log_scale]))

    start_time = time()

    n_cores = mp.cpu_count()
    with mp.Pool(processes=n_cores) as pool:
        results = pool.starmap(dynamicsOnNetworks, combinations)

    final_results = pd.concat(results, ignore_index=True)

    print("Time taken: ", time()-start_time)

    # Save the results
    final_results.to_csv(f"./temp_data/{label}.csv", index=False)
