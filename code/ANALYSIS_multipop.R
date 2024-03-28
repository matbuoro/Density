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

area_inits<-rep(NA, length(dataToJags$area))
area_inits[is.na(dataToJags$area)]<- 250 

inits<-function(){
  list(
    N=(dataToJags$DL1+dataToJags$DL2+25),
    #p=p,d=d
    #alpha= 10, beta=5,
    #p=rep(0.8,max(datatojags$year)),
    pmoy=0.7,
    delta=c(NA,0),#0.3),
    gamma=c(-1,0),#0),
    mu_kappa=0.2,
    mu_alpha=2,
    sigmaD=0.1,
    sigmaP=1,
    sigma_alpha=10,
    area=area_inits
  )
}





#ancien prior = hyperparamètres
#p[year[y]]~dbeta(2,2)
#lambda[y]<-d[year[y]]*S[y]

parameters <-c(
  #"P","p","d",
  "s",
  "kappa","alpha","beta",
  "mu_alpha","mu_kappa","mu_beta",
  "sigma_alpha","sigma_kappa","sigma_beta",
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
                n.iter = 10000*10,    # 10000 MCMC iterations + si converge pas
                n.burnin = 5000,   # discard first X iterations
                n.thin = 10) # keep every X iterations //ex: garde tous les 100 itérations

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

traplot(jagsfit, parms = c("sigmaD", "sigmaP","sigma_alpha" ,"sigma_kappa","sigma_beta"))#trois chaines, trois paramètres
#traplot(jagsfit, parms = c("muD"))#trois chaines, trois paramètres
#caterplot(jagsfit, parms = c("p"), reorder=FALSE)#trois chaines, trois paramètres
#denplot(jagsfit, parms = c("p","d")) #distrib posteriori marginales
traplot(jagsfit, parms = c("mu_kappa","mu_alpha","mu_beta","s")) #distrib posteriori marginales

traplot(jagsfit, parms = c("alpha")) #distrib posteriori marginales
caterplot(jagsfit,"alpha", reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));

caterplot(jagsfit,"kappa", reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));
caterplot(jagsfit,"beta", reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));

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

caterplot(jagsfit,paste0("Dens_pred_all[",1:max(dataToJags$popAge),"]"), reorder = FALSE, horizontal=FALSE, labels=1:max(dataToJags$popAge));title("All populations")


#caterplot(jagsfit,paste0("muD[1,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[pop])
caterplot(jagsfit,paste0("Dens_pred[1,",1:max(dataToJags$popAge),"]"), reorder = FALSE, horizontal=FALSE, labels=1:max(dataToJags$popAge));title(levels(factor(data$basin))[1])
caterplot(jagsfit,paste0("Dens_pred[2,",1:max(dataToJags$popAge),"]"), reorder = FALSE, horizontal=FALSE, labels=1:max(dataToJags$popAge));title(levels(factor(data$basin))[2])
caterplot(jagsfit,paste0("Dens_pred[3,",1:max(dataToJags$popAge),"]"), reorder = FALSE, horizontal=FALSE, labels=1:max(dataToJags$popAge));title(levels(factor(data$basin))[3])



dev.off()

