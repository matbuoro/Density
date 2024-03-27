## Not run: 
## remove (almost) everything in the working environment.
## You will get no warning, so don't do this unless you are really sure.
rm(list = ls())

# PACKAGES ####
library(R2jags)
library(mcmcplots)
load.module("glm")
#setwd("C:/Users/gmbrahy/Documents/Modele_densite/data")

## DATA ####
source("code/DATA_format.R")
str(dataToJags)

## MODEL ####
##2. Modelisation statistique: inférence des paramètres en fonction des données simulées
#langage bugs
source("code/MODEL_density.R")


## INITS ####
#commande pour lancer jags
#inits: valeurs initiales (hyperparam) chaine Markov
# N=d=p=NULL
# for (y in 1:nrow(Norvegienne)){
#   # p[y] <- (datatojags$DL2[y]+1) / (datatojags$DL1[y]+datatojags$DL2[y]+1)
#   # N[y] <- datatojags$DL1[y] / p[y]
#   # d[y] <- N[y] / datatojags$S[y]
#   d[y]<-5
#   N[y] <- d[y] * datatojags$S[y]
#   p[y] <- (datatojags$DL1[y]+1)/N[y]
# }

truc<-rep(NA, length(dataToJags$area))
truc[is.na(dataToJags$area)]<- 250 

inits<-function(){
  list(
    N=(dataToJags$DL1+dataToJags$DL2+25),
    #p=p,d=d
    #alpha= 10, beta=5,
    #p=rep(0.8,max(datatojags$year)),
    pmoy=0.7,
    delta=c(NA,0),#0.3),
    gamma=c(-1,0),#0),
    kappa=0.2,
    mu_alpha=2,
    #log_muD=rep(1,max(datatojags$year))
    #p=rbeta(max(datatojags$year),2,2),
    #d=rgamma(max(datatojags$year),1,1)
    #d=rep(0.,max(datatojags$year))
    area=truc
  )
}





#ancien prior = hyperparamètres
#p[year[y]]~dbeta(2,2)
#lambda[y]<-d[year[y]]*S[y]

parameters <-c(
  #"P","p","d",
  "kappa","alpha","mu_alpha","s",
  "sigma_alpha",
  "Dens_pred","P_pred","Dens_pred_all",
  "muD","sigmaD",
  #"alpha","beta",
  "gamma","delta",
  #"muP",
  "sigmaP",
  'muS',"sigmaS",
  "area","sigma_eps",
  "epsilonD","epsilonP"
  ) #vecteur


## JAGS ####
#appeler Jags pour compiler : données, modèle, valeurs initiales
jagsfit <- jags(dataToJags,  
                model.file = modelstat,
                parameters.to.save = parameters,  
                n.chains = 2,  # Number of chains to run.
                inits = inits,  # initial values for hyperparameters
                n.iter = 10000*1,    # 10000 MCMC iterations + si converge pas
                n.burnin = 5000*1,   # discard first X iterations
                n.thin = 1) # keep every X iterations //ex: garde tous les 100 itérations

save(jagsfit,file="results/jagsfit.Rdata")


## RESULTS ####
# Save densities estimates for each pop and age of colonisation (medians)
DensitiesByPop <- jagsfit$BUGSoutput$median$muD
colnames(DensitiesByPop) <- 1:60
rownames(DensitiesByPop) <- levels(factor(data$basin))
write.csv(round(DensitiesByPop,3), file="results/DensitiesByPop_median.csv")

pdf(file="results/MCMC.pdf")
#print(jagsfit)
#traplot(jagsfit, parms = c("sigma_eps"))#trois chaines, trois paramètres
#traplot(jagsfit, parms = c("p"))#trois chaines, trois paramètres
#caterplot(jagsfit, parms = c("p"), reorder=FALSE)#trois chaines, trois paramètres
#traplot(jagsfit, parms = c("sigmaP"))#trois chaines, trois paramètres
#traplot(jagsfit, parms = c("muP"))#trois chaines, trois paramètres

traplot(jagsfit, parms = c("sigmaD", "sigmaP","sigma_alpha"))#trois chaines, trois paramètres
#traplot(jagsfit, parms = c("muD"))#trois chaines, trois paramètres
#caterplot(jagsfit, parms = c("p"), reorder=FALSE)#trois chaines, trois paramètres
#denplot(jagsfit, parms = c("p","d")) #distrib posteriori marginales
traplot(jagsfit, parms = c("kappa","mu_alpha")) #distrib posteriori marginales

traplot(jagsfit, parms = c("alpha")) #distrib posteriori marginales
caterplot(jagsfit,"alpha", reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));

traplot(jagsfit, parms = c("gamma","delta")) #distrib posteriori marginales
denplot(jagsfit, parms = c("gamma","delta")) #distrib posteriori marginales
caterplot(jagsfit, parms = c("gamma","delta")) #distrib posteriori marginales

traplot(jagsfit, parms = c("muS","sigmaS"))#trois chaines, trois paramètres
caterplot(jagsfit,"muS", reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));
caterplot(jagsfit,"area", reorder = FALSE, horizontal=FALSE);#points(0.4)

#caterplot(jagsfit, paste0("p[",year_vec,"]"), reorder = FALSE, horizontal=FALSE);#points(0.4)
#caterplot(jagsfit, paste0("muD[",year_vec,"]"), reorder = FALSE, horizontal=FALSE);#points(0.4)
#caterplot(jagsfit,"muP", reorder = FALSE, horizontal=FALSE);#points(0.4)
#caterplot(jagsfit,"muD", reorder = FALSE, horizontal=FALSE);#points(0.4)

#caterplot(jagsfit, "N", reorder = FALSE, horizontal=FALSE);#points(N) #spécifier N
caterplot(jagsfit,paste0("epsilonD[",1:32,"]"), reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));#points(0.4)
caterplot(jagsfit,paste0("epsilonP[",1:32,"]"), reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));#points(0.4)



#caterplot(jagsfit,paste0("muD[1,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[pop])
caterplot(jagsfit,paste0("Dens_pred[1,",1:max(dataToJags$popAge),"]"), reorder = FALSE, horizontal=FALSE, labels=1:max(dataToJags$popAge));title(levels(factor(data$basin))[1])
caterplot(jagsfit,paste0("Dens_pred[2,",1:max(dataToJags$popAge),"]"), reorder = FALSE, horizontal=FALSE, labels=1:max(dataToJags$popAge));title(levels(factor(data$basin))[2])

dev.off()

