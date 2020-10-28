setwd("~/MESIO/Opt. DS/Clustering")
players <- read.csv2("players.csv", sep = ",")
players <- players[!duplicated(players[,c("short_name")]),]
rownames(players) <- players$short_name
players$player_positions <- sapply(strsplit(players$player_positions, ","), "[", 1)
players$X <- NULL
players$short_name <- NULL
players$preferred_foot <- NULL
view(players)

num_players <- players[, 3:length(players)]
view(num_players)
norm_players <- num_players/players$overall
view(norm_players)

m <- 500
n <- length(num_players)

distancies <- as.matrix(dist(head(num_players, m)))
distancies1 <- cbind(c(1:m), distancies)
distancies1 <- rbind(c(0:m), distancies1)
view(distancies1)

norm_D <- as.matrix(dist(head(norm_players, m)))
norm_D1 <- cbind(c(1:m), norm_D)
norm_D1 <- rbind(c(0:m), norm_D1)
view(norm_D1)

write.table(distancies1, file="distancies.txt", row.names=FALSE, col.names=FALSE)
write.table(norm_D1, file="nomr_distancies.txt", row.names=FALSE, col.names=FALSE)



library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)

num_player <- scale(num_players)

k <- 4


num_players_m <- head(num_player, m)
players_m <- head(players, m) 
(pos <- table(players_m$player_positions))



clust <- kmeans(num_players_m, centers = k, nstart = 25)
#Molt important treballar amb el numeric
#str(clust)

windows()
fviz_cluster(clust, data = num_players_m, geom = "point")
dev.off()

for (i in 1:k) {
  x <- players_m$player_positions[clust$cluster == i]
  print(assign(paste0("k", i), table(x)))
}


y1 <- k1
for (i in names(y1)) y1[i] <- y1[i]/pos[i]

y2 <- k2
for (i in names(y2)) y2[i] <- y2[i]/pos[i]

y3 <- k3
for (i in names(y3)) y3[i] <- y3[i]/pos[i]

y4 <- k4
for (i in names(y4)) y4[i] <- y4[i]/pos[i]

(y <- list(y1, y2, y3, y4))





library("mstknnclust")
library("stats")


#cg <- generate.complete.graph(1:nrow(matriu_players),d)
#mstree <- generate.mst(cg)
#plot(mstree$mst.graph, main="MST")

#dividirem per l'overall

res <- mst.knn(d,4)
View(res)




players_mst <- head(players_mst,500)
players_mst$cluster <- res$cluster

midgroup_mst <- players_mst[players_mst['team_position']=="MID",'cluster']
hist(midgroup_mst)

attackgroup_mst <- players_mst[players_mst['team_position']=="ATT",'cluster']
hist(attackgroup_mst)

defensegroup_mst <- players_mst[players_mst['team_position']=="DEF",'cluster']
hist(defensegroup_mst)

gkgroup_mst <- players_mst[players_mst['team_position']=="GK",'cluster']
hist(gkgroup_mst)