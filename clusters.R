library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)
players <- read.csv2("players_def.csv",row.names = 1)
dim(players)
View(players)

players <- players[!duplicated(players[,c("short_name")]),]
#aixo ens ha tret els jugadors repetits
dim(players)
players <- transform(players, row.names = short_name)
playerscluster <- players[-c(1,2)]

#Hem de modificar la fila de la posicio
playersclusters <- playerscluster[!(playerscluster[,1]=="SUB"),]
dim(playerscluster)
View(playerscluster)
playerscluster[which((playerscluster[,1]) %in% c("LB","LCB","CB","RCB","RB","RWB","LWB","SW","LB","RB")),1] = "DEF"
playerscluster[which((playerscluster[,1]) %in% c("CDM","LDM","RDM","LM","LCM","RCM","RM","CM","CAM","LAM","RAM")),1] = "MID"
playerscluster[which((playerscluster[,1]) %in% c("LW","CF","ST","RW","LS","CS","RS","LWF","RWF","LCF","RCF","RF","LF")),1] = "ATT"

k2 <- kmeans(playerscluster, centers = 4, nstart = 25)
str(k2)
