import numpy as np
import pandas as pd
import networkx as nx
import pickle
import matplotlib.pyplot as plt
import multiprocessing as mp
from tqdm import tqdm

## Networks ##
N = 2_000
f = 0.01
iterations = 2e6
n_cores = mp.cpu_count()

g1 = nx.random_regular_graph(3, N)
g2 = nx.random_regular_graph(3, N)

p_vector = np.concatenate([np.linspace(0.01, 0.1, 10), np.linspace(0.11, 0.5, 10)])
inputs = [(g1, g2, p, f, iterations) for p in p_vector]

## Functions ##
def getCoupledGraph(N, p, g1=None, g2=None):
    # Create two random regular graphs
    if g1 is None: g1 = nx.random_regular_graph(3, N)
    if g2 is None: g2 = nx.random_regular_graph(3, N)

    assert len(g1.nodes) == len(g2.nodes), "g1 and g2 must have the same number of nodes"
    assert N == len(g1.nodes), "N must correspond to the number of nodes"

    # Relabel the nodes in the second graph
    g2 = nx.relabel_nodes(g2, {i: i+N for i in g2.nodes})
    belongs_to = np.zeros(2*N)
    belongs_to[N:] = 1

    # Combine the two graphs
    g_coupled = nx.Graph()
    g_coupled.add_nodes_from(g1.nodes)
    g_coupled.add_edges_from(g1.edges)
    g_coupled.add_nodes_from(g2.nodes)
    g_coupled.add_edges_from(g2.edges)

    # Bernoulli distributed coupling
    for node in g1.nodes():
        if np.random.random() < p:
            # Choose a node from the second layer randomly
            node_in_layer2 = np.random.choice(list(g2.nodes()))
            g_coupled.add_edge(node, node_in_layer2, inter_layer=True)

    return g_coupled, belongs_to


def coupledDynamics(g, belogs_to, f, iterations, p):
    N = g.number_of_nodes()
    degree = [d for _, d in g.degree()]

    load = np.zeros(N)
    critical_load = np.array(degree)

    res = pd.DataFrame({
        "p": [],
        "iteration": [],
        "started_in": [],
        "A_0": [],
        "A_1": [],
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
            started_in = belogs_to[node]

        # Solve unstable queue
        A_0 = 0 # Avalanche size (ended in layer 0)
        A_1 = 0 # Avalanche size (ended in layer 1)
        while len(unstable_queue) > 0:
            # Pick the first node in the queue
            node = unstable_queue.pop(0)
            
            # Avalanche size counter
            toppled_in = belogs_to[node]
            if toppled_in == 0:
                A_0 += 1
            else:
                A_1 += 1

            # Get the neighbors of the node
            neighbors = list(g.neighbors(node))

            # Sample a fraction of the neighbors
            to_keep = np.random.rand(len(neighbors)) > f
            neighbors = [neighbors[i] for i in range(len(neighbors)) if to_keep[i]]

            # Update the load of the neighbors
            load[neighbors] += 1

            # Update the state of the node
            load[node] = 0

            # Check if the neighbors have become unstable
            unstable_neighbors = [n for n in neighbors if load[n] > critical_load[n]]
            if len(unstable_neighbors) > 0:
                unstable_queue.extend(unstable_neighbors)
                unstable_queue = list(set(unstable_queue))

        if A_0 > 10 or A_1 > 10:
            res.loc[len(res)] = {
                "p": p,
                "iteration": i,
                "started_in": started_in,
                "A_0": A_0,
                "A_1": A_1,
            }    

    return res


def doCoupledParallel(g1, g2, p, f, iterations):
    N = len(g1.nodes)
    g_coupled, belongs_to = getCoupledGraph(N, p, g1, g2)
    res = coupledDynamics(g_coupled, belongs_to, f, iterations, p)
    return res

## Parallel Processing ##
with mp.Pool(processes=n_cores) as pool:
    results = pool.starmap(doCoupledParallel, inputs)

## Combine the results ##
final_results = pd.concat(results, ignore_index=True)

## Save the results ##
final_results.to_csv("./temp_data/coupled2.csv", index=False)