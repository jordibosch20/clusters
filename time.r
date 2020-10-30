#compute time
start_time <- Sys.time()
#tot el codi
end_time <- Sys.time()
print(end_time-start_time)

#ens guardem els resultats

#resultats per 
temps <- c(50,100,200,300,400,500,750,1000,5000,10000)
time <- c(0.268,0.194,0.706, 0.197,0.209,0.209,0.319,0.289,0.768,1.449)
#plot(temps,time)
