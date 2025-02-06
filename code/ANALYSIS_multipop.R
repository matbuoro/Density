## Not run: 
## remove (almost) everything in the working environment.
## You will get no warning, so don't do this unless you are really sure.
rm(list = ls())

# PACKAGES ####
library(R2jags)
library(mcmcplots)
library(tidyr)
library(ggplot2)
load.module("glm")
#setwd("C:/Users/gmbrahy/Documents/Modele_densite/data")

## DATA ####
source("code/DATA_format.R")
str(dataToJags)

## MODEL ####
##2. Modelisation statistique: inférence des paramètres en fonction des données 
#langage bugs
# source("code/MODEL_density_BH.R")
source("code/MODEL_density_log.R")


## INITS ####
area_inits<-rep(NA, length(dataToJags$area))
area_inits[is.na(dataToJags$area)]<- 250 

N_inits=NULL
for (i in 1:length(dataToJags$DL1)){
N_inits[i] <- ceiling(sum(c(dataToJags$DL1[i],dataToJags$DL2[i],dataToJags$DL3[i]
  ,dataToJags$P1[i],dataToJags$P2[i],dataToJags$PE[i]
  ), na.rm = TRUE)/0.7)
}

# inits<-function(){
#   list(
#     N=N_inits,
#     #p=p,d=d
#     #alpha= 10, 
#     #beta=5,
#     #p=rep(0.8,max(datatojags$year)),
#     pmoy=0.7,
#     ppemoy=0.7,
#     #delta=c(NA,0),
#     # delta=0.5,
#     # delta_pe=0.5,
#     #gamma=c(-1,0),#0),
#     mu_kappa=15,
#     mu_alpha=2,
#     mu_beta=0.1,
#     #sigmaD=0.1,
#     sigmaP=0.5,
#     #sigmaP=c(0.5,0.5),
#     k_prior=0.5, D = 0.99,
#     kappa = runif(max(dataToJags$riverID), 10,30),
#     alpha = runif(max(dataToJags$riverID), 0,5),
#     beta = runif(max(dataToJags$riverID), 0,5),
#     #sigma_alpha=10,
#     area=area_inits,
#     tq=rep(10,max(dataToJags$riverID))#,
#     #beta=rep(0,max(dataToJags$riverID))
#   )

  # inits<-function(){ # works for logistic.
  # list(
  #   N=N_inits,
  #   pmoy=0.7,
  #   #delta=c(NA,0),#0.3),
  #   mu_kappa=30,
  #   mu_alpha=4,
  #   mu_beta=0.15,
  #   sigmaP=1,
  #   k_prior=0.5, D = 0.99,
  #   sigma_alpha= 0.3,
  #   sigma_beta=0.005,
  #   sigma_kappa=0.7,
  #   kappa = runif(max(dataToJags$riverID), 13,23),
  #   alpha = runif(max(dataToJags$riverID), 2,12),
  #   beta = runif(max(dataToJags$riverID), 0.02,0.4),
  #   #log_muD = runif(1426, 1, 23),
  #   area=area_inits,
  #   tq=rep(10,max(dataToJags$riverID))#,
  #   #beta=rep(0,max(dataToJags$riverID))
  # )
  # }
  # 
  inits<-function(){ # works for BH.
    list(
      N=N_inits,
      pmoy=0.7,
      # delta=c(NA,0),#0.3),
      mu_kappa=25,
      mu_alpha=1,
      mu_beta=1.15,
      sigmaP=1,
      #k_prior=0.5, 
      D = 0.99,
      sigma_alpha= 0.3,
      sigma_beta=0.005,
      sigma_kappa=0.7,
      kappa = runif(max(dataToJags$riverID), 13,23),
      alpha = runif(max(dataToJags$riverID), 2,12),
      beta = runif(max(dataToJags$riverID), 0.02,0.4),
      area=area_inits,
      tq=rep(10,max(dataToJags$riverID))#,
      #beta=rep(0,max(dataToJags$riverID))
    )
  }

parameters <-c("p",
  "tq","d","dens",
  "s","D",
  "kappa","alpha","beta",
  "mu_alpha","mu_kappa","mu_beta",
  "sigma_alpha","sigma_kappa","sigma_beta",
  "Dens_pred","P_pred","Dens_pred_all","P_pred_all",
  "muD","sigmaD",
  "gamma","delta",
  "sigmaP",
  'muS',"sigmaS",
  "area",
  "epsilonD","epsilonP", "tauP", "tauD", "tau_epsilon", "sigma_eps"
  ) 


## JAGS ####
#appeler Jags pour compiler : données, modèle, valeurs initiales
##Mat
jagsfit <- jags(dataToJags,  
                model.file = modelstat,
                parameters.to.save = parameters,  
                n.chains = 2,  # Number of chains to run.
                inits = inits,  # initial values for hyperparameters
                n.iter = 10000*1,   #MCMC iterations, ajouter si converge pas
                n.burnin = 2000,   # discard first X iterations
                n.thin = 1
                ) # keep every X iterations //ex: garde tous les 100 itérations

## straight
jagsfit <- jags(dataToJags,  
                model.file = modelstat,
                parameters.to.save = parameters,  
                n.chains = 2,  # Number of chains to run.
                inits = inits,  # initial values for hyperparameters
                n.iter = 5000*1,   #MCMC iterations, ajouter si converge pas
                n.burnin = 2000,   # discard first X iterations
                n.thin = 1
) 

## thinning
jagsfit <- jags(dataToJags,  
                model.file = modelstat,
                parameters.to.save = parameters,  
                n.chains = 2,  # Number of chains to run.
                inits = inits,  # initial values for hyperparameters
                n.iter = 50000*1,   #MCMC iterations, ajouter si converge pas
                n.burnin = 5000,   # discard first X iterations
                n.thin = 5
) # keep every X iterations //ex: garde tous les 100 itérations

save(jagsfit,file="results/jagsfit_log.Rdata")


## RESULTS ####
# Save densities estimates for each pop and age of colonisation (medians)
DensitiesByPop <- jagsfit$BUGSoutput$median$Dens_pred
colnames(DensitiesByPop) <- 1:50
rownames(DensitiesByPop) <- levels(factor(data$basin))
write.csv(round(DensitiesByPop,3), file="results/DensitiesByPop_median.csv")

pdf(file="results/MCMC.pdf")
print(jagsfit)
#traplot(jagsfit, parms = c("sigma_eps"))
# traplot(jagsfit, parms = c("p"))
# 
# #caterplot(jagsfit, parms = c("p"), reorder=FALSE)
traplot(jagsfit, parms = c("tau_epsilon"))
traplot(jagsfit, parms = c("sigma_eps"))
traplot(jagsfit, parms = c("sigmaP"))
traplot(jagsfit, parms = c("epsilonP"))
# traplot(jagsfit, parms = c("tauP"))
# #traplot(jagsfit, parms = c("muP"))
# 
# traplot(jagsfit, parms = c("sigmaD", "sigmaP" ,"sigma_beta","s", "delta", "p"))#trois chaines, trois paramètres
# caterplot(jagsfit, parms = c("p"), reorder=FALSE)
# 
denplot(jagsfit, parms = c("s")) #distrib posteriori marginales
# 
# traplot(jagsfit, parms = c("D")) 
# 
traplot(jagsfit, parms = c("delta")) 
caterplot(jagsfit, parms = c("delta")) 
caterplot(jagsfit, parms = c("tauP")) 
caterplot(jagsfit, parms = c("tauD"))
# 
# #estimation naive densite
# traplot(jagsfit, parms = c("muD[800]"))


# traplot(jagsfit, parms = c("beta")) #distrib posteriori marginales
traplot(jagsfit, parms = c("mu_kappa","mu_alpha","mu_beta")) #distrib posteriori marginales
# 
caterplot(jagsfit,"kappa", reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));
# caterplot(jagsfit,"d", reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));
caterplot(jagsfit,"beta", reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));
caterplot(jagsfit,"mu_alpha", reorder = FALSE, horizontal=FALSE);
caterplot(jagsfit,"muD", reorder = FALSE, horizontal=FALSE);
caterplot(jagsfit,paste0("muD[",700:722,"]"), reorder = FALSE, horizontal=FALSE); title("Densites naives")
# 
# 
# 
# traplot(jagsfit, parms = c("muS","sigmaS"))#trois chaines, trois paramètres
# caterplot(jagsfit,"muS", reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));
# caterplot(jagsfit,"tauS", reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));
# 
# caterplot(jagsfit,"area", reorder = FALSE, horizontal=FALSE);points(data$area)
# 
# 
# #caterplot(jagsfit, paste0("p[",year_vec,"]"), reorder = FALSE, horizontal=FALSE);#points(0.4)
# #caterplot(jagsfit, paste0("muD[",year_vec,"]"), reorder = FALSE, horizontal=FALSE);#points(0.4)
# #caterplot(jagsfit,"muP", reorder = FALSE, horizontal=FALSE);#points(0.4)
# #caterplot(jagsfit,"muD", reorder = FALSE, horizontal=FALSE);#points(0.4)
# 
# #caterplot(jagsfit, "N", reorder = FALSE, horizontal=FALSE);#points(N) #spécifier N
# #caterplot(jagsfit,paste0("epsilonD[",1:32,"]"), reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));#points(0.4)
# 

# caterplot(jagsfit,paste0("muD[",1:50,"]"), reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));
# caterplot(jagsfit,paste0("muD[17,",1:(dataToJags$maxPopAge[17]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[17]));title(levels(factor(data$basin))[17])

caterplot(jagsfit,paste0("P_pred[",1:51,"]"), reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin))); title("Probabilités de capture")
caterplot(jagsfit, paste0("P_pred[", 28, "]"), reorder = FALSE, horizontal = FALSE);title(levels(factor(data$basin))[28])
caterplot(jagsfit,paste0("P_pred[28,",1:32,"]"), reorder = FALSE, horizontal=FALSE);title(levels(factor(data$basin))[28])
caterplot(jagsfit,paste0("P_pred_all[",1:max(dataToJags$popAge),"]"), reorder = FALSE, horizontal=FALSE, labels=1:max(dataToJags$popAge));title("All populations")
# 
caterplot(jagsfit,paste0("epsilonP[",1:51,"]"), reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));#points(0.4)
# 
caterplot(jagsfit,paste0("Dens_pred_all[",1:max(dataToJags$popAge),"]"), reorder = FALSE, horizontal=FALSE, labels=1:max(dataToJags$popAge));title("All populations")
# 
# 
# pdf(file="Dens_pred_trueMaxPopAge_log_straight.pdf")
#pdf(file="Dens_pred_trueMaxPopAge_log_thinning.pdf")
pdf(file="Dens_pred_trueMaxPopAge_BH_thinning.pdf")
par(mfrow=c(2,1))
###very handy to brows through all populations predictions. Use click, forward only atm.
for (i in 1:length(trueMaxPopAge)){
    a<-paste0("Dens_pred[" ,  as.character(i) , ","  , 1:(dataToJags$trueMaxPopAge[i]) ,  "]"  )
    caterplot(jagsfit, a ,  reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$trueMaxPopAge[i]));title(levels(factor(data$basin))[i])
    locator(1)
}
dev.off()
# 
# 
# par(mfrow=c(2,2))
# #caterplot(jagsfit,paste0("muD[1,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[pop])
# caterplot(jagsfit,paste0("Dens_pred[1,",1:(dataToJags$maxPopAge[1]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[1]));title(levels(factor(data$basin))[1])
# caterplot(jagsfit,paste0("Dens_pred[2,",1:(dataToJags$maxPopAge[2]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[2]));title(levels(factor(data$basin))[2])
# caterplot(jagsfit,paste0("Dens_pred[22,",1:(dataToJags$maxPopAge[5]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[5]));title(levels(factor(data$basin))[5])
# caterplot(jagsfit,paste0("Dens_pred[18,",1:(dataToJags$maxPopAge[19]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[19]));title(levels(factor(data$basin))[19])
# caterplot(jagsfit,paste0("Dens_pred[18,",1:(dataToJags$maxPopAge[15]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[15]));title(levels(factor(data$basin))[15])
# 
# caterplot(jagsfit,paste0("Dens_pred[10,",1:max(dataToJags$popAge),"]"), reorder = FALSE, horizontal=FALSE, labels=1:max(dataToJags$popAge));title(levels(factor(data$basin))[10])
# #caterplot(jagsfit,paste0("Dens_pred[27,",1:max(dataToJags$popAge),"]"), reorder = FALSE, horizontal=FALSE, labels=1:max(dataToJags$popAge));title(levels(factor(data$basin))[27])
# #caterplot(jagsfit,paste0("Dens_pred[21,",1:max(dataToJags$popAge),"]"), reorder = FALSE, horizontal=FALSE, labels=1:max(dataToJags$popAge));title(levels(factor(data$basin))[21])
# 
# 
# 
# dev.off()
# 
# library(bayesplot)
# posterior <- jagsfit$BUGSoutput$sims.matrix
# # Convert MCMC samples to data frame
# #posterior <- as.data.frame(as.mcmc(do.call(rbind, mcmc_samples)))
# mcmc_pairs(posterior, pars = c("delta[1]","delta[2]", "gamma[1]", "gamma[2]"),off_diag_args = list(size = 1.5))





# muD <- (jagsfit$BUGSoutput$sims.list$muD)
# pop <- 1
# mypop <- muD[,pop,1:(dataToJags$maxPopAge[pop])] # uniquement pop
# 
# medians <- apply(mypop,2,median)
# q2.5 <- apply(mypop,2,quantile, probs=0.025)
# q97.5 <- apply(mypop,2,quantile, probs=0.975)
# q25 <- apply(mypop,2,quantile, probs=0.25)
# q75 <- apply(mypop,2,quantile, probs=0.75)
# 
# #observedPop <- 1:(dataToJags$maxPopAge[pop]) # all years
# observedPop <- c(12,42,43)
# plot(NULL, xlim=c(1,50),ylim=c(0,30), main=levels(factor(data$basin))[pop])
# points(observedPop,medians[observedPop], pch=16)
# segments(observedPop,q2.5[observedPop],observedPop,q97.5[observedPop])
# segments(observedPop,q25[observedPop],observedPop,q75[observedPop], lwd=2)



#plot medianes: quantiles avec valeurs de coldate/popage en x ?


pdf(file="Observed_densityByPop.pdf")
par(mfrow=c(1,1))

#Estimations de densités sur base des données 
medianes_muD <- (jagsfit$BUGSoutput$median$muD)
plot(as.factor(data$year_capture),jagsfit$BUGSoutput$median$muD)
plot(as.factor(data$year_capture)[data$basin=="Chateau"],jagsfit$BUGSoutput$median$muD[data$basin=="Chateau"])


# Identifier les valeurs uniques dans la colonne 'basin'
unique_basins <- unique(data$basin)
# pdf(file="Observed_densityByPop_naive.pdf")


pdf(file="Observed_densityByPop.pdf")
par(mfrow=c(2,1))
# Boucle sur chaque valeur unique de 'basin'
for (basin_name in unique_basins) {
  # Filtrer les données pour le basins actuel
  basin_data <- data[data$basin == basin_name, ]
  
  # Créer le plot pour ce basin
  plot(as.factor(basin_data$year_capture+1), 
       jagsfit$BUGSoutput$median$muD[data$basin == basin_name], 
       main = paste("Plot for basin:", basin_name),
       xlab = "Pop Age", 
       ylab = "Median estimated muD (fish/100 m^2", pch = 19)

}
dev.off()



# plot(data = muD_est %>% filter(riverID == 1) , x = year_capture2, y = muD)

plot(factor(data$year_capture+1),muD)
plot(factor(data$year_capture+1),muD[])
#regarder pour la pop [1], aux points d'échantillonage (selon popAge)
caterplot(jagsfit,paste0("Dens_pred[1,",1:(dataToJags$maxPopAge[1]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[1]));title(levels(factor(data$basin))[1])

pdf(file="Observed_densityByPop.pdf")
par(mfrow=c(2,1))
dens <- (jagsfit$BUGSoutput$sims.list$dens)
for (pop in unique(dataToJags$riverID)){
ids <- which(dataToJags$riverID==pop)
densPop<-as.matrix(dens[,ids])#Toutes les estimations de densité (16000 lignes) pour la pop sélectionnée
  
medians <- apply(densPop,2,median)
q2.5 <- apply(densPop,2,quantile, probs=0.025)
q97.5 <- apply(densPop,2,quantile, probs=0.975)
q25 <- apply(densPop,2,quantile, probs=0.25)
q75 <- apply(densPop,2,quantile, probs=0.75)

observedPop <- (dataToJags$year[ids])
plot(NULL, xlim=c(1,56),ylim=c(0,100), ylab="Densité/100 m^2",xaxt='n', xlab="",main=levels(factor(data$basin))[pop])
axis(1,at=1:56,labels=1970:2025, las=2, cex.txt=0.75)
points(observedPop,medians, pch=16)
segments(observedPop,q2.5,observedPop,q97.5)
segments(observedPop,q25,observedPop,q75, lwd=2)
}
dev.off()
