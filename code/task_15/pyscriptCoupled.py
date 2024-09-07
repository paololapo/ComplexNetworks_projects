import numpy as np
import pandas as pd
import networkx as nx
import pickle
import matplotlib.pyplot as plt
import multiprocessing as mp
from tqdm import tqdm
from sandpileDynamics import coupledDynamics, getCoupledGraph

## Networks and parameters ##
N = 2_000
f = 0.01

iterations = 5e7
repetitions = 8 # Number of repetitions for each p
n_cores = mp.cpu_count()

p_vector = np.concatenate([np.linspace(0.01, 0.1, 8), np.linspace(0.11, 0.5, 8)])


## Function ##
def doCoupledParallel(g1, g2, p, f, iterations):
    """
    Run the coupled sandpile model in parallel and save the results.
    """
    N = len(g1.nodes)
    g_coupled, belongs_to = getCoupledGraph(N, p, g1, g2)
    res = coupledDynamics(g_coupled, belongs_to, f, iterations, p)
    return res


## Parallel Processing ##
results_list = []
for i in range(repetitions):
    g1 = nx.random_regular_graph(3, N)
    g2 = nx.random_regular_graph(3, N)
    inputs = [(g1, g2, p, f, iterations) for p in p_vector]

    with mp.Pool(processes=n_cores) as pool:
        results = pool.starmap(doCoupledParallel, inputs)

    results_list.extend(results)

## Combine the results ##
final_results = pd.concat(results_list)

## Save the results ##
final_results.to_csv("./temp_data/coupled.csv", index=False)
