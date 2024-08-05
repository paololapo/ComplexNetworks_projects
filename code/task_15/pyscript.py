import numpy as np
import pandas as pd
import networkx as nx
import pickle
import matplotlib.pyplot as plt
import multiprocessing as mp
from tqdm import tqdm

## Networks ##
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
def dynamics(g, f, iterations):
    N = g.number_of_nodes()
    degree = [d for _, d in g.degree()]

    load = np.zeros(N)
    critical_load = np.array(degree)

    res = pd.DataFrame({
        "iteration": [],
        "S": [],
        "T": [],
        "G": [],
        "A": []
    })

    # Run the simulation
    for i in tqdm(range(int(iterations))):
        unstable_queue = []

        # Select a random node
        node = np.random.randint(N)
        
        # Update the load of the selected node
        load[node] += 1

        # Check if the node has become unstable
        if load[node] > critical_load[node]:
            unstable_queue.append(node)

        # Solve unstable queue
        S = []
        T = 1
        G = 0
        while len(unstable_queue) > 0:
            # Pick the first node in the queue
            node = unstable_queue.pop(0)
            S.append(node)

            # Get the neighbors of the node
            neighbors = list(g.neighbors(node))

            # Sample a fraction of the neighbors
            to_keep = np.random.rand(len(neighbors)) > f
            neighbors = [neighbors[i] for i in range(len(neighbors)) if to_keep[i]]
            G = G + len(neighbors)

            # Update the load of the neighbors
            load[neighbors] += 1

            # Update the state of the node
            load[node] = 0

            # Check if the neighbors have become unstable
            unstable_neighbors = [n for n in neighbors if load[n] > critical_load[n]]
            if len(unstable_neighbors) > 0:
                unstable_queue.extend(unstable_neighbors)
                unstable_queue = list(set(unstable_queue))
                T += 1

        A = list(set(S))

        if (len(A) > 0):
            res.loc[len(res)] = {
                "iteration": i,
                "S": len(S),
                "T": T,
                "G": G,
                "A": len(A)
            }    

    return res

def doParallel(g, label, f, iterations):
    res = dynamics(g, f, iterations)
    
    # Save the results
    res.to_csv(f"./temp_data/{label}.csv")
    
    # Save the network
    with open(f"./temp_data/{label}.gpickle", "wb", pickle.HIGHEST_PROTOCOL) as f:
        pickle.dump(g, f)


## Parallel Processing ##
with mp.Pool(processes=n_cores) as pool:
    results = pool.starmap(doParallel, inputs)
