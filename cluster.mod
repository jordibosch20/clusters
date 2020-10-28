# Parameters
param m; # number of players
set M := {1..m};
param k; # number of clusters

param d {M, M};

# Variables
var x {M, M}, binary;

# Model
minimize dist_median: sum{i in M, j in M} d[i, j]*x[i,j];

subject to point_belongs_cluster{i in M}: sum{j in M} x[i, j] = 1;
subject to exactly_k_cluster: sum{j in M} x[j, j] = k;
subject to cluster_j_contains_point_j {i in M, j in M, i != j}: x[j, j] >= x[i, j]; 