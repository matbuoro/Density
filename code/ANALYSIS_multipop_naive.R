## Not run: 
## remove (almost) everything in the working environment.
## You will get no warning, so don't do this unless you are really sure.
rm(list = ls())

invlogit<-function(x) {1/(1+exp(-(x)))}
logit<-function(x) {log(x/(1-x))}


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
source("code/MODEL_density_naive.R")


## INITS ####
area_inits<-rep(NA, length(dataToJags$area))
area_inits[is.na(dataToJags$area)]<- 250 

N_inits=NULL
for (i in 1:length(dataToJags$DL1)){
  N_inits[i] <- ceiling(sum(c(dataToJags$DL1[i],dataToJags$DL2[i],dataToJags$DL3[i]
                              ,dataToJags$P1[i],dataToJags$P2[i],dataToJags$PE[i]
  ), na.rm = TRUE)/0.7)
}


inits<-function(){ # works for naive
  list(
    N=N_inits,
    #pmoy=0.7,
    sigmaP=1,
    # sigmaS
    # sigmaD
    area=area_inits
  )
}

parameters <-c("muD"
                ,"alpha_muD"
                ,"pmoy"
               ,"P_pred"
               ,"sigma_eps"
               ,"sigmaD"
               ,"delta"
               ,"sigmaP"
               ,'muS',"sigmaS"
               #,"area"
               #"epsilonD","epsilonP", "tauP", "tauD", "tau_epsilon", "sigma_eps"
) 


## JAGS ####
#appeler Jags pour compiler : données, modèle, valeurs initiales
##Mat
jagsfit <- jags(dataToJags,  
                model.file = modelstat,
                parameters.to.save = parameters,  
                n.chains = 2,  # Number of chains to run.
                inits = inits,  # initial values for hyperparameters
                n.iter = 5000*1,   #MCMC iterations, ajouter si converge pas
                n.burnin = 1000,   # discard first X iterations
                n.thin = 1
) # keep every X iterations //ex: garde tous les 100 itérations

save(jagsfit,file="results/jagsfit_naive.Rdata")


## RESULTS ####
# Save densities estimates for each pop and age of colonisation (medians)
# DensitiesByPop <- jagsfit$BUGSoutput$median$Dens_pred
# colnames(DensitiesByPop) <- 1:50
# rownames(DensitiesByPop) <- levels(factor(data$basin))
# write.csv(round(DensitiesByPop,3), file="results/DensitiesByPop_median.csv")

pdf(file="results/MCMC_naive.pdf")
#print(jagsfit)
traplot(jagsfit, parms = c("delta"))
traplot(jagsfit, parms = c("sigma_eps"))
traplot(jagsfit, parms = c("sigmaP"))
traplot(jagsfit, parms = c("sigmaD"))
traplot(jagsfit, parms = c("sigmaS"))

caterplot(jagsfit, parms = c("P_pred"), reorder=FALSE, horizontal = FALSE, labels=levels(factor(data$basin))); title("Proba capture");
caterplot(jagsfit, parms = c("muS"), reorder=FALSE, horizontal = FALSE, labels=levels(factor(data$basin))); title("Moyenne surface");

dev.off()




muD <- jagsfit$BUGSoutput$sims.list$muD
str(muD)
quantiles <- apply(muD, 2, function(x) quantile(x, probs = c(0.05, 0.25,0.5, 0.75, 0.95)))

riverIDs <- sort(unique(dataToJags$riverID))
pdf(file="results/Observed_densityByPop.pdf")
par(mfrow=c(2,1))
#dens <- (jagsfit$BUGSoutput$sims.list$dens)
for (pop in riverIDs){
  ids <- which(dataToJags$riverID==pop)
  observedPop<- dataToJags$year[ids]
  #observedPop<- 1:MaxPopAge[pop]
  #observedPop<- 1:max(dataToJags$maxPopAge)
  #observedPop<- sort(unique(tmp))-1962
  q50 <-quantiles["50%",ids]
  q5 <-quantiles["5%",ids]
  q95 <-quantiles["95%",ids]
  q25 <-quantiles["25%",ids]
  q75 <-quantiles["75%",ids]
  plot(NULL, xlim=c(1,59),ylim=c(0,25), ylab="Densité/100 m^2",xaxt='n', xlab="",main=levels(factor(data$basin))[pop])
  axis(1,at=1:59,labels=1967:2025, las=2, cex=0.25)
  points(observedPop,q50, pch=16)
  segments(observedPop,q5,observedPop,q95)
  segments(observedPop,q25,observedPop,q75, lwd=2)
}
dev.off()





alpha_muD <- jagsfit$BUGSoutput$sims.list$alpha_muD
str(alpha_muD)

# Assuming alpha_muD is already loaded in the environment
quantiles <- apply(alpha_muD, c(2, 3), function(x) quantile(x, probs = c(0.05, 0.25,0.5, 0.75, 0.95)))
# This returns an array of size [3, 51, 63] where:
# - The first dimension (size 3) corresponds to the quantiles (5%, 50%, 95%)
# - The second dimension (size 51) corresponds to populations
# - The third dimension (size 63) corresponds to the 3rd dimension indices
quantiles <- aperm(quantiles, c(1, 3, 2))  # Change from (3, 51, 63) to (3, 63, 51)

#MaxPopAge<-dataToJags$maxPopAge
# Assuming `quantiles` is the [3, 51, 63] array from the previous step
# and `trueMaxPopAge` is a vector of length 51

riverIDs <- sort(unique(dataToJags$riverID))
# # by MetapopAge
# for (pop in riverIDs) {
#   if(MaxPopAge[pop]==63) next;
#   quantiles[, (dataToJags$maxPopAge[pop] + 1):63, pop] <- NA
# }

#by AgePop
# for (pop in riverIDs) {
#   if(MaxPopAge[pop]==63) next;
#   quantiles[, (MaxPopAge[pop] + 1):63, pop] <- NA
# }


# plot(factor(data$year_capture+1),muD)
# plot(factor(data$year_capture+1),muD[])
# #regarder pour la pop [1], aux points d'échantillonage (selon popAge)
# caterplot(jagsfit,paste0("Dens_pred[1,",1:(dataToJags$maxPopAge[1]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[1]));title(levels(factor(data$basin))[1])

pdf(file="results/Observed_densityByPop.pdf")
par(mfrow=c(2,1))
#dens <- (jagsfit$BUGSoutput$sims.list$dens)
for (pop in riverIDs){
  #ids <- which(dataToJags$riverID==pop)
  #observedPop <- 1:dataToJags$trueMaxPopAge[pop]#(dataToJags$year[ids])
  tmp<-dataToJags$year[dataToJags$riverID==pop]
  #observedPop<- 1:MaxPopAge[pop]
  #observedPop<- 1:max(dataToJags$maxPopAge)
  observedPop<- sort(unique(tmp))-1962
  q5 <-quantiles["5%",observedPop,pop]
  q95 <-quantiles["95%",observedPop,pop]
  q25 <-quantiles["25%",observedPop,pop]
  q75 <-quantiles["75%",observedPop,pop]
  plot(NULL, xlim=c(1,63),ylim=c(0,25), ylab="Densité/100 m^2",xaxt='n', xlab="",main=levels(factor(data$basin))[pop])
  axis(1,at=1:63,labels=1963:2025, las=2, cex=0.25)
  points(observedPop,quantiles["50%",observedPop,pop], pch=16)
  segments(observedPop,q5,observedPop,q95)
  segments(observedPop,q25,observedPop,q75, lwd=2)
}
dev.off()



