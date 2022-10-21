#!/usr/bin/env python
# coding: utf-8
import sys
import re
import pickle

from dask_ml.cluster import KMeans
import feather
import numpy as np

job = sys.argv[1]
rep = int(re.findall('\[(\d+)\]', job)[0])

df = feather.read_dataframe('data.feather')
clusterer = KMeans(1370, tol=0.000001, max_iter=30000, n_jobs=-1, init='k-means++')
clusterer.fit(df.values)

#with open(f'results/clusterer_{rep}.pkl', 'wb') as f:
#    pickle.dump([
#        clusterer.cluster_centers_,
#        clusterer.inertia_], f)

np.save(f"results/centroids_{rep}.npy", clusterer.cluster_centers_)
np.save(f"results/inertia_{rep}.npy", clusterer.inertia_)

# Read pickle
# with open(f'clusterer_{rep}.pkl', 'rb') as f:
#     centroids, inertia = pickle.load(f)
