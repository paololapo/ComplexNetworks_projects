import numpy as np
import pandas as pd
import networkx as nx
import pickle
import matplotlib.pyplot as plt
import multiprocessing as mp
from tqdm import tqdm
from sandpileDynamics import dynamics

## Networks and parameters ##
N = 10_000
f = 1/N
iterations = 1e7
n_cores = mp.cpu_count()

networks = [
    nx.barabasi_albert_graph(N, 5), 
    nx.barabasi_albert_graph(N, 1),
    nx.erdos_renyi_graph(N, 0.0007),
    nx.erdos_renyi_graph(N, 0.002),
    nx.scale_free_graph(N, 0.1, 0.1, 0.8).to_undirected(),
    nx.scale_free_graph(N, 0.35, 0.3, 0.35).to_undirected(),
    nx.scale_free_graph(N, 0.4, 0.1, 0.5).to_undirected(),
    nx.scale_free_graph(N, 0.2, 0.7, 0.1).to_undirected(),
]

labels = [
    "BA_5", 
    "BA_1", 
    "ER_0-0007", 
    "ER_0-002", 
    "SF_0-1_0-1_0-8", 
    "SF_0-35_0-3_0-35", 
    "SF_0-4_0-1_0-5", 
    "SF_0-2_0-7_0-1"
]

inputs = [(networks[i], labels[i], f, iterations) for i in range(len(networks))]


## Functions ##
def doParallel(g, label, f, iterations):
    """
    Run the sandpile model in parallel and save the results.
    """
    res = dynamics(g, f, iterations)
    
    # Save the results
    res.to_csv(f"./temp_data/{label}.csv")
    
    # Save the network
    with open(f"./temp_data/{label}.gpickle", "wb", pickle.HIGHEST_PROTOCOL) as f:
        pickle.dump(g, f)


## Parallel Processing ##
with mp.Pool(processes=n_cores) as pool:
    results = pool.starmap(doParallel, inputs)
