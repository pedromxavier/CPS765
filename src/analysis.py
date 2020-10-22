## Standard Library
import os
import csv
import sys
import time

clock = time.perf_counter

ARGV = sys.argv
ARGC = len(ARGV)

## Third-party
from graph_tool.all import Graph
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sn

for arg in ARGV[1:]:
    t = clock()

    g = Graph()
    g.load(file_name=f'../data/{arg}', fmt='gt')
    
    print(f'Data Loaded. Elapsed time: {clock() - t:.2f}s', end='\n')
    v = g.get_vertices()
    e = g.get_edges()
    print(f'Vertices = {len(v)}; Edges = {len(e)}', end='\n')

    deg = g.get_total_degrees(v)
    
    min_deg = np.min(deg)
    max_deg = np.max(deg)
    avg_deg = np.mean(deg)
    med_deg = np.median(deg)
    std_deg = np.std(deg)

    print(f'''Degree:
    min = {min_deg:.3f}
    max = {max_deg:.3f}
    avg = {avg_deg:.3f} ± {std_deg:.3f}
    med = {med_deg:.3f}''')

    print(f"Total Elapsed time: {clock() - t:.2f}s")
    
    del g