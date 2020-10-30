##### Lecture and manipulation of our data ######


setwd("~/MESIO/Opt. DS/Clustering")
players <- read.csv2("players.csv", sep = ",")
players <- players[!duplicated(players[,c("short_name")]),]
rownames(players) <- players$short_name
players$player_positions <- sapply(strsplit(players$player_positions, ","), "[", 1)
players$X <- NULL
players$short_name <- NULL
players$preferred_foot <- NULL

# Taking only the numeric variables
num_players <- players[, 3:length(players)]

# Dividing the players' stats by their overall rating to scale the data
sc_players <- num_players/players$overall

# Number of players
m <- 500

# Distance matrix
D <- as.matrix(dist(head(sc_players, m)))
D1 <- cbind(c(1:m), D)
D1 <- rbind(c(0:m), D1)

# Saving the distance matrix
write.table(D1, file="distancies.txt", row.names=FALSE, col.names=FALSE)



###### k-means ######

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)

# Desired number of clusters
k <- 4

players_m <- head(players, m) 
sc_players_m <- head(sc_players, m)

# Number of players per position
(pos <- table(players_m$player_positions))

# Applying k-means
clust <- kmeans(sc_players_m, centers = k, nstart = 25)

# Visualization of the clusters
windows()
fviz_cluster(clust, data = sc_players_m, geom = "point")

#Visualization of each cluster and each FIFA position
for (i in 1:k) {
  x <- players_m$player_positions[clust$cluster == i]
  print(assign(paste0("k", i), table(x)))
}

for (i in 1:k) {
  assign(paste0("prop", i), get(paste0("k", i)))
  for (name in names(get(paste0("k", i)))) {
    assign(paste0("prop", i), assign_in(get(paste0("prop", i)), name, get(paste0("k", i))[name]/pos[name]))
  }
  print(get(paste0("prop", i)))
}

for (i in names(pos)) {
  clustini <- 1
  aux <- vector("integer")
  assign(i, aux)
  for (kk in y) {
    for (name in names(kk)) {
      if (name == i) {
        aux <- c(aux, rep(clustini, kk[i]*pos[i]))
      }
    }
    clustini <- clustini + 1
  }
  assign(i, factor(aux, levels = c(1:4)))
}



###### MST ######

library(mstknnclust)
library(stats)
library(igraph)

# We create a weighted graph given the distance matrix 
cg <- graph.adjacency(D, mode = "undirected", weighted = TRUE)

# Computation of the minimum spanning tree
mstree <- minimum.spanning.tree(cg)

# Elimination of the k - 1 heaviest edges of our spanning tree to obtain 
# the k clusters
for (i in 1:(k - 1)) {
  mstree <- delete_edges(mstree, which.max(edge_attr(mstree)$weight))
}

# Obtaining the k clusters and their players
str(comp <- components(mstree))






