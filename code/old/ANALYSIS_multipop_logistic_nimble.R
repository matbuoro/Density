## Not run: 
## remove (almost) everything in the working environment.
## You will get no warning, so don't do this unless you are really sure.
rm(list = ls())

invlogit<-function(x) {1/(1+exp(-(x)))}
logit<-function(x) {log(x/(1-x))}


# PACKAGES ####
#library(R2jags)
library(mcmcplots)
library(tidyr)
library(ggplot2)
#load.module("glm")
library(nimble)
#setwd("C:/Users/gmbrahy/Documents/Modele_densite/data")

## DATA ####
source("code/DATA_format.R")
attach(dataToJags)

dataToNimble <-list(
  DL1=DL1
  ,DL2=DL2
)

constants <-list(n=n
                 ,n1=n1
                 #,n2=n2
                 #,n3=n3
                 ,riverID=riverID
                 #,year=year
                 ,popAge=popAge
                 #,censusType=censusType
                 #,coldate=coldate
                 #,maxPopAge=maxPopAge
                 #,maxMetapopAge=maxMetapopAge
)

## MODEL ####
##2. Modelisation statistique: inférence des paramètres en fonction des données 
#langage bugs
# source("code/MODEL_density_BH.R")
source("code/MODEL_density_log_nimble.R")
#source("code/MODEL_density_naive_nimble.R")

## INITS ####
area_inits<-dataToJags$area
area_inits[is.na(area_inits)]<- 250 
#dataToJags$area <- area_inits

N_inits=dens=NULL
for (i in 1:dataToJags$n1[2]){
  #for (i in 1:900){
  N_inits[i] <- ceiling(sum(c(dataToJags$DL1[i],dataToJags$DL2[i],dataToJags$DL3[i]
                              #,dataToJags$P1[i],dataToJags$P2[i],dataToJags$PE[i]
  ), na.rm = TRUE)/0.7)
  
  dens[i]<-N_inits[i]/(area_inits[i]/100)
}

area <- dataToJags$area
area_inits<-rep(NA, length(area))
area_inits[is.na(area)]<- 250 
#area_inits <- area_inits[1:dataToJags$n1[2]]


inits<-function(){ # works for naive
  list(
    #n1=dataToJags$n1
    N=N_inits
    , dens=dens
    , delta=invlogit(0.7)
    , sigmaP=0.8
    , sigmaD=0.7
    , sigmaS=0.9
    , sigma_eps=0.4
    , kappa=exp(3)
    #, pre_kappa=0.5
    #, sigma_kappa=0.1
    , alpha=3
    , beta=0.1
    , area=area_inits
    , r=2
  )
}

parameters <-c("muD"
               ,"muD_pred"
               ,"kappa"
               #,"mu_kappa","sigma_kappa"
               ,"alpha"
               ,"beta"
               ,"pmoy"
               ,"P_pred"
               ,"sigma_eps"
               ,"sigmaD"
               ,"delta"
               ,"sigmaP"
               ,'muS',"sigmaS"
               #,"area"
               ,"r"
               ,"t50"
               #"epsilonD","epsilonP", "tauP", "tauD", "tau_epsilon", "sigma_eps"
) 



# RUN MCMC ####
n_chains <- 2 # number of chains
n_store <- 5000 # target of number of iteration to store per chain
n_burnin <- 1000 # number of iterations to discard
n_thin <- 1 # thinning interval
n_iter <- (n_store * n_thin) + n_burnin # number of iterations to run per chain
print(n_iter)

samples <- nimbleMCMC(code = modelstat,     # model code
                      data = dataToNimble,                  # data
                      constants =constants,        # constants
                      inits = inits,          # initial values
                      monitors = parameters,   # parameters to monitor
                      WAIC=FALSE,                      #waic
                      niter = n_iter,                  # nb iterations
                      nburnin = n_burnin,              # length of the burn-in
                      nchains = n_chains,              # nb of chains
                      thin = n_thin,                   # thinning interval (default = 1)
                      samplesAsCodaMCMC=T
)

save(samples,file="results/MCMC_nimble_log.Rdata")


## RESULTS ####
# Save densities estimates for each pop and age of colonisation (medians)
# DensitiesByPop <- jagsfit$BUGSoutput$median$Dens_pred
# colnames(DensitiesByPop) <- 1:50
# rownames(DensitiesByPop) <- levels(factor(data$basin))
# write.csv(round(DensitiesByPop,3), file="results/DensitiesByPop_median.csv")

pdf(file="results/MCMC_naive_All.pdf")
#print(jagsfit)
traplot(jagsfit, parms = c("delta"))
traplot(jagsfit, parms = c("sigma_eps"))
traplot(jagsfit, parms = c("sigmaP"))
traplot(jagsfit, parms = c("sigmaD"))
traplot(jagsfit, parms = c("sigmaS"))
traplot(jagsfit, parms = c("r"))

denplot(jagsfit,"pmoy")

#caterplot(jagsfit, parms = c("Kappa"), reorder=FALSE, horizontal = FALSE, labels=levels(factor(data$basin))); title("Proba capture");

caterplot(jagsfit, parms = c("P_P_pred"), reorder=FALSE, horizontal = FALSE, labels=levels(factor(data$basin))); title("Proba capture");
#caterplot(jagsfit, parms = c("delta"), reorder=FALSE, horizontal = TRUE, labels=c(1,2,3)); title("Delta");
caterplot(jagsfit, parms = c("muS"), reorder=FALSE, horizontal = FALSE, labels=levels(factor(data$basin))); title("Moyenne surface");

dev.off()




muD <- jagsfit$BUGSoutput$sims.list$muD
str(muD)
area <- jagsfit$BUGSoutput$sims.list$area
str(area)
quantiles <- apply(muD, 2, function(x) quantile(x, probs = c(0.05, 0.25,0.5, 0.75, 0.95)))
quantiles_area <- apply(area, 2, function(x) quantile(x, probs = c(0.05, 0.25,0.5, 0.75, 0.95)))

riverIDs <- sort(unique(dataToJags$riverID))
pdf(file="results/Observed_densityByPop_naive.pdf")
par(mfrow=c(2,1))
#dens <- (jagsfit$BUGSoutput$sims.list$dens)
for (pop in riverIDs){
  ids <- which(dataToJags$riverID==pop)
  observedPop<- dataToJags$year[ids]
  #observedPop<- 1:MaxPopAge[pop]
  #observedPop<- 1:max(dataToJags$maxPopAge)
  #observedPop<- sort(unique(tmp))-1962
  q50_area <-quantiles_area["50%",ids]
  q5_area <-quantiles_area["5%",ids]
  q95_area <-quantiles_area["95%",ids]
  q25_area <-quantiles_area["25%",ids]
  q75_area <-quantiles_area["75%",ids]
  q50 <-quantiles["50%",ids]
  q5 <-quantiles["5%",ids]
  q95 <-quantiles["95%",ids]
  q25 <-quantiles["25%",ids]
  q75 <-quantiles["75%",ids]
  # colors <- ifelse(dataToJags$is.there.area[ids] == "No", "red", "black")
  plot(NULL, xlim=c(1,59),ylim=c(0,35), ylab="Densité/100 m^2",xaxt='n', xlab="",main=levels(factor(data$basin))[pop])
  axis(1,at=1:59,labels=1967:2025, las=2, cex=0.25)
  points(observedPop,q50, pch=16)#, col=colors)
  segments(observedPop,q5,observedPop,q95)
  segments(observedPop,q25,observedPop,q75, lwd=2)
  # legend("topright", legend = c("No", "Yes"), col = c("red", "black"), pch = 16, bty = "n", cex = 0.8, title = "Area")
  # Tracer les valeurs de 'area' à droite
  # par(new = TRUE)  # Créer un nouvel axe sur la droite du graphique
  # plot(observedPop, q50_area, type = "p", pch = 1, col = "blue", xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "", xlim = c(1, 59), ylim = c(0, 2500))
  # segments(observedPop, q5_area, observedPop, q95_area, col = "blue")
  # segments(observedPop, q25_area, observedPop, q75_area, col = "blue", lwd = 2)
  # 
  # # Ajouter un axe à droite pour 'area'
  # axis(4, at = seq(0, 2500, by = 500), labels = seq(0, 2500, by = 500), col = "blue", col.axis = "blue")
  # mtext("Area", side = 4, line = 3, col = "blue")
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

# pdf(file="results/Observed_densityByPop_naive.pdf")
par(mfrow=c(2,1))
#dens <- (jagsfit$BUGSoutput$sims.list$dens)
for (pop in riverIDs){
  #ids <- which(dataToJags$riverID==pop)
  #observedPop <- 1:dataToJags$trueMaxPopAge[pop]#(dataToJags$year[ids])
  # tmp<-dataToJags$year[dataToJags$riverID==pop]
  tmp<-dataToJags$popAge[dataToJags$riverID==pop]
  #observedPop<- 1:MaxPopAge[pop]
  #observedPop<- 1:max(dataToJags$maxPopAge)
  # observedPop<- sort(unique(tmp))-1962
  observedPop<- sort(unique(tmp))
  q5 <-quantiles["5%",observedPop,pop]
  q95 <-quantiles["95%",observedPop,pop]
  q25 <-quantiles["25%",observedPop,pop]
  q75 <-quantiles["75%",observedPop,pop]
  # plot(NULL, xlim=c(1,63),ylim=c(0,25), ylab="Densité/100 m^2",xaxt='n', xlab="popAge",main=levels(factor(data$basin))[pop])
  # plot(NULL, xlim=c(1,((max(data$year[data$riverID==pop])-min(data$year[data$riverID==pop])+1))),ylim=c(0,25), ylab="Densité/100 m^2",xaxt='n', xlab="popAge",main=levels(factor(data$basin))[pop])
  # plot(NULL, xlim=1:max(dataToJags$popAge[pop]),ylim=c(0,25), ylab="Densité/100 m^2",xaxt='n', xlab="popAge",main=levels(factor(data$basin))[pop])
  # plot(NULL, xlim=c(1,(max(data$popAge[data$riverID==pop])+2025-max(data$year[data$riverID==pop]))),ylim=c(0,25), ylab="Densité/100 m^2",xaxt='n', xlab="popAge",main=levels(factor(data$basin))[pop])
  # plot(NULL, xlim=c(1,max(data$popAge[data$riverID==pop])),ylim=c(0,25), ylab="Densité/100 m^2",xaxt='n', xlab="popAge",main=levels(factor(data$basin))[pop])
  
  plot(NULL, xlim=c(1,59),ylim=c(0,25), ylab="Densité/100 m^2",xaxt='n', xlab="",main=levels(factor(data$basin))[pop])
  axis(1,at=1:59,labels=1967:2025, las=2, cex=0.25)
  # axis(1,at=1:63,labels=1963:2025, las=2, cex=0.25)
  # axis(1,at=1:((max(data$year[data$riverID==pop])-min(data$year[data$riverID==pop])+1)),labels=min(data$year[data$riverID==pop]):max(data$year[data$riverID==pop]), las=2, cex=0.25)
  # axis(1,at=min(dataToJags$popAge[pop]):max(dataToJags$popAge[pop]),labels=min(dataToJags$popAge[pop]):max(dataToJags$popAge[pop]), las=2, cex=0.25)
  # axis(1,at=1:max(data$popAge[data$riverID==pop]),labels=1:max(data$popAge[data$riverID==pop]), las=2, cex=0.25)
  
  points(observedPop,quantiles["50%",observedPop,pop], pch=16)
  segments(observedPop,q5,observedPop,q95)
  segments(observedPop,q25,observedPop,q75, lwd=2)
}
dev.off()



