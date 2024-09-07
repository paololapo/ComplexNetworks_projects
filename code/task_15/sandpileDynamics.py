import numpy as np
import pandas as pd
import networkx as nx
import pickle
import matplotlib.pyplot as plt
import multiprocessing as mp
from tqdm import tqdm


def dynamics(g, f, iterations):
    """
    Simulate the dynamics of the sandpile model on a network.
    Args:
        g (nx.Graph): The networkx graph.
        f (float): Dissipation rate.
        iterations (int): Number of iterations to run the simulation.
    """

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


def getCoupledGraph(N, p, g1=None, g2=None):
    """
    Create a coupled graph from two random regular graphs.
    Args:
        N (int): Number of nodes in each graph.
        p (float): Probability of inter-layer edges.
        g1 (nx.Graph): The first graph. If None, a random regular graph is created.
        g2 (nx.Graph): The second graph. If None, a random regular graph is created.
    """
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
    stubs_layer1 = [n for n in list(g1.nodes()) if np.random.random() < p]
    stubs_layer2 = [n for n in list(g2.nodes()) if np.random.random() < p]

    np.random.shuffle(stubs_layer1)
    np.random.shuffle(stubs_layer2)

    for i, j in zip(stubs_layer1, stubs_layer2):
        g_coupled.add_edge(i, j, inter_layer=True)
    
    # for node in g1.nodes():
    #     if np.random.random() < p:
    #         # Choose a node from the second layer randomly
    #         node_in_layer2 = np.random.choice(list(g2.nodes()))
    #         g_coupled.add_edge(node, node_in_layer2, inter_layer=True)

    return g_coupled, belongs_to


def coupledDynamics(g, belogs_to, f, iterations, p):
    """
    Simulate the dynamics of the sandpile model on a coupled network.
    Args:
        g (nx.Graph): The networkx graph.
        belogs_to (np.array): The layer of each node.
        f (float): Dissipation rate.
        iterations (int): Number of iterations to run the simulation.
        p (float): Probability of inter-layer edges (used as label).
    """
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

        # Save only avalanches larger than 10
        if A_0 > 10 or A_1 > 10:
            res.loc[len(res)] = {
                "p": p,
                "iteration": i,
                "started_in": started_in,
                "A_0": A_0,
                "A_1": A_1,
            }    

    return res



    