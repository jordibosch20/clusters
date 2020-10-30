setwd("~/MESIO/Opt. DS/Clustering")

m <- 500

# Reading and manipulating the ampl solution
ampl <- read.delim("sol_ampl.txt", sep = "")
sol_ampl <- ampl[ampl$X0 != 0,]
sol_ampl$X0 <- NULL

sol <- sol_ampl[1:m,]

sol[, 20:m] <- NA

for (i in 1:floor(m/19)) {
  for (j in 1:m) {
    for (k in 1:19) sol[j, 19*i + k] <- sol_ampl[i*m + j, k]
  }
}
sol[, (m + 1): length(sol)] <- NULL
names(sol) <- rownames(players_m)

# Which players are the medoids of each cluster?
(medoids <- which(sapply(sol, sum) > 0))
med_players <- names(medoids)

# Table with number of players per cluster
for (i in medoids) {
  x <- players_m$player_position[sol[, i] == 1]
  print(assign(med_players[which(medoids == i)], table(x)))
}

# Proportion
for (i in 1:k) {
  assign(paste0("prop_am", i), get(names(medoids)[i]))
  for (name in names(get(paste0("prop_am", i)))) {
    assign(paste0("prop_am", i), assign_in(get(paste0("prop_am", i)), name, get(paste0("prop_am", i))[name]/pos[name]))
  }
  print(get(paste0("prop_am", i)))
}

# Comprobacio de que la funció objectiu ens dona lo del ampl
f_obj <- 0
for (i in medoids) {
  print(f_obj <- f_obj + sum(norm_D[i,sol[, i] == 1]))
}
f_obj
