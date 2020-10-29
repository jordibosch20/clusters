setwd("~/MESIO/Opt. DS/Clustering")
players <- read.csv2("players.csv", sep = ",")
players <- players[!duplicated(players[,c("short_name")]),]
rownames(players) <- players$short_name
players$player_positions <- sapply(strsplit(players$player_positions, ","), "[", 1)
players$X <- NULL
players$short_name <- NULL
players$preferred_foot <- NULL
View(players)

num_players <- players[, 3:length(players)]
View(num_players)
norm_players <- num_players/players$overall
View(norm_players)

m <- 500
n <- length(num_players)

distancies <- as.matrix(dist(head(num_players, m)))
distancies1 <- cbind(c(1:m), distancies)
distancies1 <- rbind(c(0:m), distancies1)
View(distancies1)

norm_D <- as.matrix(dist(head(norm_players, m)))
norm_D1 <- cbind(c(1:m), norm_D)
norm_D1 <- rbind(c(0:m), norm_D1)
View(norm_D1)

write.table(distancies1, file="distancies.txt", row.names=FALSE, col.names=FALSE)
write.table(norm_D1, file="nomr_distancies.txt", row.names=FALSE, col.names=FALSE)



library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)

num_player <- scale(num_players)

k <- 4


num_players_m <- head(norm_players, m)
players_m <- head(players, m) 
(pos <- table(players_m$player_positions))



clust <- kmeans(num_players_m, centers = k, nstart = 25)
#Molt important treballar amb el numeric
#str(clust)

quartz()
fviz_cluster(clust, data = num_players_m, geom = "point")
dev.off()

for (i in 1:k) {
  x <- players_m$player_positions[clust$cluster == i]
  print(assign(paste0("k", i), table(x)))
}

#ATTACK
y1 <- k1
for (i in names(y1)) y1[i] <- y1[i]/pos[i]

#DEFENSE
y2 <- k2
for (i in names(y2)) y2[i] <- y2[i]/pos[i]

#GK
y3 <- k3
for (i in names(y3)) y3[i] <- y3[i]/pos[i]

#MIDFIELDER
y4 <- k4
for (i in names(y4)) y4[i] <- y4[i]/pos[i]

(y <- list(y1, y2, y3, y4))
y <- y[c(4,3,1,2)]


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
levels(CAM) <- c("GK","DEF","MID","ATT")
levels(CB) <- c("GK","DEF","MID","ATT")
levels(CDM) <- c("GK","DEF","MID","ATT")
levels(CF) <- c("GK","DEF","MID","ATT")
levels(CM) <- c("GK","DEF","MID","ATT")
levels(GK) <- c("GK","DEF","MID","ATT")
levels(LB) <- c("GK","DEF","MID","ATT")
levels(LM) <- c("GK","DEF","MID","ATT")
levels(LW) <- c("GK","DEF","MID","ATT")
levels(LWB) <- c("GK","DEF","MID","ATT")
levels(RB) <- c("GK","DEF","MID","ATT")
levels(RM) <- c("GK","DEF","MID","ATT")
levels(RW) <- c("GK","DEF","MID","ATT")
levels(RWB) <- c("GK","DEF","MID","ATT")
levels(ST) <- c("GK","DEF","MID","ATT")



#Aqui farem els plots de cada posicio amb el seu 
quartz(width = 10, height = 10)                # Linux: x11(); macOS: quartz()
par(mfrow = c(3, 3), las = 1, font.main = 4, font.lab = 4, font.axis = 2,
    oma = c(0, 0, 1, 0), mar = c(3, 4, 4, 2))

plot(GK, col=c("#FF9900","#FFFF33","#0066CC","#FF3300"))
title(main="GK")

plot(CB, col=c("#FF9900","#FFFF33","#0066CC","#FF3300"))
title(main="CB ")

plot(CDM, col=c("#FF9900","#FFFF33","#0066CC","#FF3300"))
title(main="CDM ")

#les posem totes a la mateixa
plot(LB, col=c("#FF3300","#FFFF33","#FF9900","#0066CC"))
title(main="LB/LBW/RB/RBW ")

plot(CM, col=c("#FF9900","#FFFF33","#0066CC","#FF3300"))
title(main="CM ")

plot(LM, col=c("#FF9900","#FFFF33","#0066CC","#FF3300"))
title(main="LM/RM ")

plot(CAM, col=c("#FF9900","#FFFF33","#0066CC","#FF3300"))
title(main="CAM")

plot(LW, col=c("#FF9900","#FFFF33","#0066CC","#FF3300"))
title(main="LW/RW")

plot(ST, col=c("#FF3300","#FFFF33","#FF9900","#0066CC"))
title(main="ST/CF")


dev.off()
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