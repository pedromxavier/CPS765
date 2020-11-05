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
from graph_tool.topology import label_components, shortest_distance
## import matplotlib.pyplot as plt
import numpy as np
## import seaborn as sn

def components(g: Graph):
    comp, *_ = label_components(g, directed=g.is_directed())
    com = np.zeros(np.max(comp.a) + 1, dtype=int)
    np.add.at(com, comp.a, 1)
    return com

def distances(g: Graph):
    dist = shortest_distance(g, directed=g.is_directed())
    return np.array([[y for y in x] for x in dist], dtype=int)

def report(a: np.ndarray):
    min_a = np.min(a)
    max_a = np.max(a)
    avg_a = np.mean(a)
    med_a = np.median(a)
    std_a = np.std(a)
    return f'''  range = [{min_a:.1f}, {max_a:.1f}]
    avg = {avg_a:.1f} ± {std_a:.1f}
    med = {med_a:.1f}'''

def get_graph(fname: str) -> Graph:
    fdir = os.path.join(f'..', f'data') 
    fpath = os.path.join(fdir, name)
    if os.path.exists(fpath):
        g = Graph()
        g.load(file_name=f'../data/{name}', fmt='gt')
        return g
    else:
        raise FileNotFoundError('Invalid Graph, options are:\n' + '\n'.join(os.listdir(fdir)))

def analysis(name: str):
    t = clock()
    g = Graph()
    g.load(file_name=f'../data/{name}', fmt='gt')
    
    print(f'Dados carregados. Tempo: {clock() - t:.2f}s', end='\n')
    v = g.get_vertices()
    e = g.get_edges()

    # Degree
    deg = g.get_total_degrees(v)
    deg_rpt = report(deg)

    # Connected Components
    com = components(g)
    com_rpt = report(com)
    td = clock()
    # Distances
    dis = distances(g)
    dis_rpt = report(dis)
    print(f'td = {clock() - td}')
    
    print(f'Vértices: {len(v)}; Arestas: {len(e)}; Componentes Conexas: {len(com)};', end='\n--\n')

    print(f'Grau dos vértices:\n{deg_rpt}', end='\n--\n')

    print(f'Tamanho das componentes conexas:\n{com_rpt}', end='\n--\n')

    print(f'Distâncias:\n{dis_rpt}', end='\n--\n')

    print(f"Tempo total: {clock() - t:.2f}s")

for name in ARGV[1:]:
    print(f'\n--------- {name:^20s} ---------\n')
    analysis(name)
