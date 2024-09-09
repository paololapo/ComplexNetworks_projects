import numpy as np
import networkx as nx
import matplotlib.pyplot as plt
import pandas as pd
import multiprocessing as mp
import itertools
from time import time

from dynamics import *


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