import numpy as np
import bioscience as bs
import gc
import pandas as pd
from sklearn.datasets import make_biclusters
import sys

def calculate_sparsity(array):
    # Convert the input to a NumPy array if it's not already
    array = np.asarray(array)
    
    # Get the total number of elements
    total_elements = array.size
    
    # Count the number of non-zero elements
    non_zero_elements = np.count_nonzero(array)
    
    # Calculate sparsity
    sparsity = (total_elements - non_zero_elements) / total_elements
    
    return sparsity

if len(sys.argv) < 3:
    print("Usage: python bioscience_experiment.py <dataset_path> <mode>")
    sys.exit(1)

dataset_path = sys.argv[1]
mode = int(sys.argv[2])

# 1000 bij 100, duurt ~30 seconden op cpu (single core)
# 2400 bij 100, duurt ~1 seconde op gpu
# 2500 bij 100, duurt ineens oneindig op gpu, vreemd wel.
data, rows, cols = make_biclusters(shape=(2000, 1200), n_clusters=3, random_state=42)

# Change the 'rate' to add create more missing values, increasing the sparsity
# data = add_random_missing(data, 0.1)

# Comment this line to use new data
data = pd.read_csv(dataset_path, sep=',').values

# Zet de numpy array die sklearn geeft om in een dataset van BioScience.
lengths = None
geneNames = None
colNames = [str(i) for i in range(len(cols[0]))]

dataset_GPU = bs.Dataset(data, geneNames=geneNames, columnsNames=colNames, lengths=lengths)
dataset_CPU = bs.Dataset(data, geneNames=geneNames, columnsNames=colNames, lengths=lengths)

# Calculate sparsity
# Use this value to reason about sparsity difference between datasets.
sparsity = calculate_sparsity(dataset_GPU.data)
print(f"Sparsity: {sparsity:.4f}")

# RNA-Seq preprocessing
bs.tpm(dataset_GPU)

# Binary preprocessing
bs.binarize(dataset_GPU)

# Data mining phase
if mode == 3:
    listModels = bs.bibit(dataset_GPU, cMnr=2, cMnc=2, mode=3, deviceCount=1, debug = True)
else:
    listModels = bs.bibit(dataset_GPU, cMnr=2, cMnc=2, mode=mode, debug = True)

del listModels
del dataset_GPU
gc.collect()

