library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)
players <- read.csv2("players_def.csv",row.names = 1)

players <- players[!duplicated(players[,c("short_name")]),]
#aixo ens ha tret els jugadors repetits
dim(players)
players <- transform(players, row.names = short_name)
playerscluster <- players[-c(1,2)]

#Hem de modificar la fila de la posicio
playerscluster <- playerscluster[!(playerscluster[,1]=="SUB"),]
dim(playerscluster)

playerscluster[which((playerscluster[,1]) %in% c("LB","LCB","CB","RCB","RB","RWB","LWB","SW","LB","RB")),1] = "DEF"
playerscluster[which((playerscluster[,1]) %in% c("CDM","LDM","RDM","LM","LCM","RCM","RM","CM","CAM","LAM","RAM")),1] = "MID"
playerscluster[which((playerscluster[,1]) %in% c("LW","CF","ST","RW","LS","CS","RS","LWF","RWF","LCF","RCF","RF","LF")),1] = "ATT"
playerscluster <- playerscluster[complete.cases(playerscluster), ]

#Buscar manera d'entrenar el model amb totes les dades
#Pero a l'hora de fer lultim plot nomes surtin els jugadors que ens interessen
playerscluster <- playerscluster

sapply(playerscluster,class)
#Hem d'applicar el k-means sense el valor de character
players_numeric = playerscluster[-c(1)]

players_numeric <- na.omit(players_numeric)
players_numeric <- scale(players_numeric)

#players_numeric <- head(players_numeric,8000)

k2 <- kmeans(players_numeric, centers = 4, nstart = 25)
#Molt important treballar amb el numeric
str(k2)

quartz()
#marc tu canvia quart per la instruccio amb windows
fviz_cluster(k2, data = players_numeric, geom="text")
dev.off()

#Comprovacio dels 4 grups
#Afegim la nova llista al dataframe
#playerscluster <- head(playerscluster,8000)
playerscluster$cluster <- k2$cluster

quartz(width = 10, height = 10)                # Linux: x11(); macOS: quartz()
par(mfrow = c(2, 2), las = 1, font.main = 4, font.lab = 4, font.axis = 2,
    oma = c(0, 0, 1, 0), mar = c(3, 4, 4, 2))

midgroup <- playerscluster[playerscluster['team_position']=="MID",'cluster']
hist(midgroup)

attackgroup <- playerscluster[playerscluster['team_position']=="ATT",'cluster']
hist(attackgroup)

defensegroup <- playerscluster[playerscluster['team_position']=="DEF",'cluster']
hist(defensegroup)

gkgroup <- playerscluster[playerscluster['team_position']=="GK",'cluster']
hist(gkgroup)


