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
attach(dataToJags)

## MODEL ####
##2. Modelisation statistique: inférence des paramètres en fonction des données 
#langage bugs
# source("code/MODEL_density_BH.R")
source("code/MODEL_density_log.R")


## INITS ####

area_inits<-dataToJags$area
area_inits[is.na(area_inits)]<- 250 
#dataToJags$area <- area_inits

N_inits=dens=NULL
for (i in 1:dataToJags$n){
  #for (i in 1:900){
  N_inits[i] <- ceiling(sum(c(dataToJags$DL1[i],dataToJags$DL2[i],dataToJags$DL3[i]
                              ,dataToJags$P1[i],dataToJags$P2[i],dataToJags$PE[i]
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
    , sigmaP=0.5
    , sigma_eps=0.5
    , sigmaD=1
    , kappa=20
    #, kappa=rep(exp(2.5), max(dataToJags$riverID))
    #, mu_kappa=20
    #, sigma_kappa=.1
    , alpha=7
    , beta=0.1
    , area=area_inits
    #, r=10
  )
}

parameters <-c("dens","muD"
               ,"muD_pred"
               ,"kappa"
               #,"mu_kappa","sigma_kappa"
               ,"alpha"
               ,"beta"
               ,"pmoy"
               ,"P_pred"
               ,"epsilonP"
               ,"sigma_eps"
               ,"sigmaD"
               ,"delta"
               ,"sigmaP"
               ,'muS',"sigmaS"
               ,"area","nu","sigma_nu"
               #,"r"
               ,"C_pred"
               ,"t50"
) 


## JAGS ####
#appeler Jags pour compiler : données, modèle, valeurs initiales
##Mat
jagsfit <- jags(dataToJags,  
                model.file = modelstat,
                parameters.to.save = parameters,  
                n.chains = 3,  # Number of chains to run.
                inits = inits,  # initial values for hyperparameters
                n.iter = 5000*10,   #MCMC iterations, ajouter si converge pas
                n.burnin = 1000,   # discard first X iterations
                n.thin = 10
) # keep every X iterations //ex: garde tous les 100 itérations

save(jagsfit,file="results/jagsfit_logistic_kappaCst.Rdata")
#save(jagsfit,file="results/jagsfit_logistic_kappaVariable.Rdata")

## RESULTS ####
# Save densities estimates for each pop and age of colonisation (medians)
# DensitiesByPop <- jagsfit$BUGSoutput$median$Dens_pred
# colnames(DensitiesByPop) <- 1:50
# rownames(DensitiesByPop) <- levels(factor(data$basin))
# write.csv(round(DensitiesByPop,3), file="results/DensitiesByPop_median.csv")

pdf(file="results/MCMC_logistic_kappaCst.pdf")
#pdf(file="results/MCMC_logistic_kappaVariable.pdf")
par(mfrow=c(1,1))
#print(jagsfit)
par <- c(
 #"kappa"
 "sigma_kappa","mu_kappa"
#,"r"
,"t50"
,"alpha"
,"beta"#,"sigma_beta"
,"pmoy"
,"sigma_eps"
,"sigmaD"
,"delta"
,"sigmaP"
#,'muS'
,"sigmaS"
,"nu","sigma_nu"
)
traplot(jagsfit, parms = par)
# traplot(jagsfit, parms = c("sigma_eps"))
# traplot(jagsfit, parms = c("sigmaP"))
# traplot(jagsfit, parms = c("sigmaD"))
# traplot(jagsfit, parms = c("sigmaS"))
#denplot(jagsfit,"alpha")
denplot(jagsfit,"kappa")
denplot(jagsfit,"t50")

caterplot(jagsfit, parms = c("P_pred"), reorder=FALSE, horizontal = FALSE, labels=levels(factor(data$basin))); title("Proba capture");
caterplot(jagsfit, parms = c("muS"), reorder=FALSE, horizontal = FALSE, labels=levels(factor(data$basin))); title("Moyenne surface");
#caterplot(jagsfit, parms = c("kappa"), reorder=FALSE, horizontal = FALSE, labels=levels(factor(data$basin))); title("Kappa");
#caterplot(jagsfit, parms = c("beta"), reorder=FALSE, horizontal = FALSE, labels=levels(factor(data$basin))); title("Moyenne surface");
caterplot(jagsfit, parms = c("muD_pred"), reorder=FALSE, horizontal = FALSE); title("Densité # temps");
caterplot(jagsfit, parms = c("epsilonP"), reorder=FALSE, horizontal = FALSE,labels=levels(factor(data$basin))); title("epsilon capture");

#caterplot(jagsfit, parms = c("area"), reorder=FALSE, horizontal = FALSE)


parToPlot <- c(
  "kappa"
  #"sigma_kappa","mu_kappa"
  #,"r"
  #,"t50"
  ,"alpha"
  ,"beta"#,"sigma_beta"
  #,"pmoy"
  ,"sigma_eps"
  ,"sigmaD"
  ,"delta"
  ,"sigmaP"
  #,'muS'
  ,"sigmaS"
  ,"nu","sigma_nu"
)


library(corrplot)  # For correlation plot

# Convert MCMC list to matrix
samples_mat <- as.matrix(jagsfit$BUGSoutput$sims.matrix) 

# Get parameter names from samples
param_names <- colnames(samples_mat)

# Find parameters that match any in `parToPlot` (including multi-dimensional ones)
matching_params <- param_names[grepl(paste0("^(", paste(parToPlot, collapse = "|"), ")\\[?\\d*\\]?$"), param_names)]
#matching_params <- c(matching_params,"h2[1]","h2[2]")
# Check if we found matching parameters
if (length(matching_params) == 0) {
  stop("No matching parameters found. Check parameter names!")
}

# Extract only the relevant parameters
filtered_samples <- samples_mat[, matching_params, drop = FALSE]


# Compute correlation matrix
cor_matrix <- cor(filtered_samples)

# Plot the correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7)



# Posterior check
C_pred <- jagsfit$BUGSoutput$sims.list$C_pred
# C <- c(dataToJags_DL$DL1+dataToJags_DL$DL2
#         ,dataToJags_DL$P1+dataToJags_DL$P2
#         ,dataToJags_PE)
C=NULL
for (i in 1:n){
  #  for (i in dataToJags$n3[1]:dataToJags$n3[2]){
  C[i] <- ceiling(sum(c(
    DL1[i],DL2[i]#,DL3[i]
    , P1[i]#,P2[i]
    , PE[i]
  ), na.rm = TRUE))
}

# Plot the posterior predictive check
plot(NULL, xlim=c(0,200),ylim=c(0,200), ylab="Predict", xlab="Observed",main="Captures")
abline(0,1)
points(C,apply(C_pred,2,quantile,probs=0.5), pch=16)
segments(C,apply(C_pred,2,quantile,probs=0.05),C,apply(C_pred,2,quantile,probs=0.95))

# Plot the posterior predictive check
plot(NULL, xlim=c(0,200),ylim=c(0,200), ylab="Predict", xlab="Observed",main="Captures")
abline(0,1)
iter <- sample(1:nrow(C_pred), 10)
for(i in iter){
  points(C,C_pred[i,], pch=16, col=rgb(0,0,0,0.25))
}
# for(j in 1:1917){
# text(C[j],C_pred[2,j], labels=j, pos=4, cex=0.5)
# }


# for (pop in 1:max(dataToJags$riverID)){
#   caterplot(jagsfit, paste0("muD_pred[",1:50,",",pop,"]"), reorder=FALSE, horizontal = FALSE); title(levels(factor(data$basin))[pop]);
#   id_pop <- which(dataToJags$riverID==pop)
#   popAge<-dataToJags$popAge[id_pop]
#   
#   #muD
#   muD <- jagsfit$BUGSoutput$sims.list$muD[,id_pop]
#   if(length(id_pop)==1){
#     muD_quantiles <- quantile(muD, probs = c(0.05, 0.25,0.5, 0.75, 0.95))
#     points(popAge, muD_quantiles["50%"], pch=16)
#     segments(popAge, muD_quantiles["5%"], popAge, muD_quantiles["95%"])
#     segments(popAge, muD_quantiles["25%"], popAge, muD_quantiles["75%"], lwd=2)
#     
#     # dens
#     points(popAge, dens_quantiles["50%"], pch=16, col="pink")
#     segments(popAge, dens_quantiles["5%"], popAge, dens_quantiles["95%"], col="pink")
#     segments(popAge, dens_quantiles["25%"], popAge, dens_quantiles["75%"], lwd=2, col="pink")
#   }else{
#     muD_quantiles <-apply(muD, 2, function(x) quantile(x, probs = c(0.05, 0.25,0.5, 0.75, 0.95)))
#     points(popAge, muD_quantiles["50%",], pch=16)
#     segments(popAge, muD_quantiles["5%",], popAge, muD_quantiles["95%",])
#     segments(popAge, muD_quantiles["25%",], popAge, muD_quantiles["75%",], lwd=2)
#     
#     #Dens
#     dens <- jagsfit$BUGSoutput$sims.list$dens[,id_pop]
#     dens_quantiles <-apply(dens, 2, function(x) quantile(x, probs = c(0.05, 0.25,0.5, 0.75, 0.95)))
#     points(popAge, dens_quantiles["50%",], pch=16, col="pink")
#     segments(popAge, dens_quantiles["5%",], popAge, dens_quantiles["95%",], col="pink")
#     segments(popAge, dens_quantiles["25%",], popAge, dens_quantiles["75%",], lwd=2, col="pink")
#   }
# 
#   }

dev.off()






# muD <- jagsfit$BUGSoutput$sims.list$muD
# str(muD)
# quantiles <- apply(muD, 2, function(x) quantile(x, probs = c(0.05, 0.25,0.5, 0.75, 0.95)))
# 
# riverIDs <- sort(unique(dataToJags$riverID))
# pdf(file="results/Observed_densityByPop_log.pdf")
# par(mfrow=c(2,1))
# #dens <- (jagsfit$BUGSoutput$sims.list$dens)
# for (pop in riverIDs){
#   ids <- which(dataToJags$riverID==pop)
#   observedPop<- dataToJags$year[ids]
#   #observedPop<- 1:MaxPopAge[pop]
#   #observedPop<- 1:max(dataToJags$maxPopAge)
#   #observedPop<- sort(unique(tmp))-1962
#   q50 <-quantiles["50%",ids]
#   q5 <-quantiles["5%",ids]
#   q95 <-quantiles["95%",ids]
#   q25 <-quantiles["25%",ids]
#   q75 <-quantiles["75%",ids]
#   plot(NULL, xlim=c(1,59),ylim=c(0,35), ylab="Densité/100 m^2",xaxt='n', xlab="",main=levels(factor(data$basin))[pop])
#   axis(1,at=1:59,labels=1967:2025, las=2, cex=0.25)
#   points(observedPop,q50, pch=16)
#   segments(observedPop,q5,observedPop,q95)
#   segments(observedPop,q25,observedPop,q75, lwd=2)
# }
# dev.off()





#alpha_muD <- jagsfit$BUGSoutput$sims.list$alpha_muD
#str(alpha_muD)

# Assuming alpha_muD is already loaded in the environment
#quantiles <- apply(alpha_muD, c(2, 3), function(x) quantile(x, probs = c(0.05, 0.25,0.5, 0.75, 0.95)))
# This returns an array of size [3, 51, 63] where:
# - The first dimension (size 3) corresponds to the quantiles (5%, 50%, 95%)
# - The second dimension (size 51) corresponds to populations
# - The third dimension (size 63) corresponds to the 3rd dimension indices
#quantiles <- aperm(quantiles, c(1, 3, 2))  # Change from (3, 51, 63) to (3, 63, 51)

#MaxPopAge<-dataToJags$maxPopAge
# Assuming `quantiles` is the [3, 51, 63] array from the previous step
# and `trueMaxPopAge` is a vector of length 51

#riverIDs <- sort(unique(dataToJags$riverID))
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

# pdf(file="results/Observed_densityByPop.pdf")
# par(mfrow=c(2,1))
# #dens <- (jagsfit$BUGSoutput$sims.list$dens)
# for (pop in riverIDs){
#   #ids <- which(dataToJags$riverID==pop)
#   #observedPop <- 1:dataToJags$trueMaxPopAge[pop]#(dataToJags$year[ids])
#   tmp<-dataToJags$year[dataToJags$riverID==pop]
#   #observedPop<- 1:MaxPopAge[pop]
#   #observedPop<- 1:max(dataToJags$maxPopAge)
#   observedPop<- sort(unique(tmp))-1962
#   q5 <-quantiles["5%",observedPop,pop]
#   q95 <-quantiles["95%",observedPop,pop]
#   q25 <-quantiles["25%",observedPop,pop]
#   q75 <-quantiles["75%",observedPop,pop]
#   plot(NULL, xlim=c(1,63),ylim=c(0,25), ylab="Densité/100 m^2",xaxt='n', xlab="",main=levels(factor(data$basin))[pop])
#   axis(1,at=1:63,labels=1963:2025, las=2, cex=0.25)
#   points(observedPop,quantiles["50%",observedPop,pop], pch=16)
#   segments(observedPop,q5,observedPop,q95)
#   segments(observedPop,q25,observedPop,q75, lwd=2)
# }
# dev.off()



