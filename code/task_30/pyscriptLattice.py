import numpy as np
import networkx as nx
import matplotlib.pyplot as plt
import pandas as pd

import multiprocessing as mp
import itertools

from time import time

## Functions ##
def cultural_similarity(features, i, j):
    """
    Compute the cultural similarity (feature overlap) between two nodes
    """
    return np.sum(features[i,] == features[j,])


def cultural_differences(features, i, j):
    """
    Get the indices of the different features between two nodes
    """
    return np.where(features[i] != features[j])[0]


def edge_similarities(edges, features):
    """
    Compute the cultural similarity between all edges in the graph
    """
    similarities = [cultural_similarity(features, i, j) for i, j in edges]
    return similarities


def single_step(edges, features, F):
    """
    Perform a single step of the model
    """
    # Randomly select an edge
    edge = edges[np.random.randint(len(edges))]

    # Get the nodes
    node_i, node_j = edge

    # Compute the cultural similarity
    cs = cultural_similarity(features, node_i, node_j)

    # Compute the probability of interaction
    p = cs / F

    # Interact with probability p
    if (np.random.rand() < p) and (p < 1):
        # Get the different features
        diff = cultural_differences(features, node_i, node_j)

        # Randomly select a different feature
        feature_to_change = diff[np.random.randint(len(diff))]

        # Update the features
        features[node_j, feature_to_change] = features[node_i, feature_to_change]
   
    return features


def get_cultural_domains(g, features):
    """
    Get the cultural domains of the graph
    """
    # Get the unique features
    unique_features = np.unique(features, axis=0)

    sizes = []

    # Iterate over the unique features
    for feature in unique_features:
        # Get the nodes with the feature
        nodes = np.where((features == feature).all(axis=1))[0]

        # Get the subgraph
        subgraph = g.subgraph(nodes)

        # Find the connected components
        components = list(nx.connected_components(subgraph))

        # Get the size of the components
        sizes.extend([len(c) for c in components])

    return sizes


def get_s_max(g, features):
    """
    Get the largest cultural domains
    """
    sizes = get_cultural_domains(g, features)
    return max(sizes)


def get_n_active_bonds(edges, features, F):
    """
    Get the number of active bonds
    """
    similarities = edge_similarities(edges, features)
    eg = np.array(similarities)
    active_bounds = eg[(eg > 0) & (eg < F)]

    return len(active_bounds)


## Dynamics ##
def dynamics(L, F, q, rep_mc, max_iterations, log_scale):
    local_res = pd.DataFrame({"mc": [], "L": [], "F": [], "q": [], "s_max_den": [], "n_density": [], "iteration": []})

    # Repeat the Monte Carlo simulation
    for mc in range(rep_mc):
        print(f"Processing Monte Carlo {mc}, L={L}, F={F}, q={q}")

        # Create the graph and initialize the features
        g = nx.grid_graph([L, L])
        mapping = {(i, j): i * L + j for i in range(L) for j in range(L)}
        g = nx.relabel_nodes(g, mapping)

        N = g.number_of_nodes()
        edges = list(g.edges())
        n_edges = g.number_of_edges()
        features = np.random.poisson(q, (N, F))

        for j in range(int(max_iterations)+1):
            features = single_step(edges, features, F)

            # Save the results
            if j in log_scale:
                s_max_den = get_s_max(g, features) / N
                n_density = get_n_active_bonds(edges, features, F) / n_edges

                local_res.loc[len(local_res)] = {
                    "mc": mc,
                    "L": L,
                    "F": F,
                    "q": q,
                    "s_max_den": s_max_den,
                    "n_density": n_density,
                    "iteration": j,
                }

                print(f"Iteration {j}, s_max_den={s_max_den}, n_density={n_density}")

    return local_res


## Parallel Processing ##
L_list = [50]
F_list = [2, 5, 10]
q_list = [1, 10, 100, 200, 300, 500]
rep_mc = 1
max_iterations = 1e10

log_scale = np.unique(np.logspace(0, np.log10(1e10), 96, base=10, endpoint=True, dtype=int))

combinations = list(itertools.product(L_list, F_list, q_list, [rep_mc], [max_iterations], [log_scale]))

start_time = time()

n_cores = mp.cpu_count()
with mp.Pool(processes=n_cores) as pool:
    results = pool.starmap(dynamics, combinations)

final_results = pd.concat(results, ignore_index=True)

print("Time taken: ", time()-start_time)

# Save the results
final_results.to_csv("resultspy3.csv", index=False)