rm(list = ls(all = TRUE))

setwd("/home/gustavo/Dropbox/Documentos/Maestria_CCA/Proyecto_tesis/SACM/Datos/Por_year/10min")

#system.time(DF9317 <- read.csv("precip10_93-17.csv", header = T, stringsAsFactors = F))
DF9317 <- read.csv("precip10_93-17_BS.csv", header = T, stringsAsFactors = F)
DF9317 <- replace(DF9317,is.na(DF9317),0)

#=============================

for (est in 5:65) {
  print(paste("Estacion", est - 4))
  
  ##==================== codigo
  
  m <- 1:14 
  # tau(10)=1hr*2^10=1024hr    --> 960hr=42.66dias       Para tau0=1hr
  # tau(13)=10min2^13=81920min --> 1365.3hr=56.8dias     Para tau0=10min
  n <- as.vector(m)
  
  
  DF9317[, c(est)] <- as.numeric(unlist(DF9317[, c(est)]))
  
  for (i in m) {
    n[[i]] <- length(DF9317[, c(5)]) / (2 ^ (i - 1))
  }
  
  tau <- as.vector(m)
  tau[] <- 0
  for (i in m) {
    tau[i] <- 10 * 2 ^ (i - 1)
  }
  
  ##============== codigo intermitencia
  X <- as.vector(m);  X[] <- NA
  for (j in m) {
    print(j)
    B <- 0
    RI <- as.vector(1:round(n[j])); RI[] <- NA
    for(i in 1:(round(n[j])-1)){
      RI[i] <- (DF9317[i,c(est)] - DF9317[(i+(2^(j-1))), c(est)])*(6/2^(j-1)) # mm/hr
    }
    B <- sum(abs(RI/(1/6))^2*(1/6),na.rm=T)
    X[j] <- B/tau[j]
  }
  
  write.table(X, paste("X_BS_est_", est - 4, ".csv", sep = ""), row.names = FALSE, col.names = c(paste("X_est", est - 4, sep = "_")))
  
}
