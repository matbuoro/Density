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

N_inits=NULL
for (i in 1:length(dataToJags$DL1)){
N_inits[i] <- ceiling(sum(c(dataToJags$DL1[i],dataToJags$DL2[i],dataToJags$DL3[i]
  ,dataToJags$P1[i],dataToJags$P2[i]
 # ,dataToJags$PE[i]
  ), na.rm = TRUE)/0.7)
}

inits<-function(){
  list(
    N=N_inits,
    #p=p,d=d
    #alpha= 10, beta=5,
    #p=rep(0.8,max(datatojags$year)),
    pmoy=0.7,
    delta=c(NA,0),#0.3),
    #gamma=c(-1,0),#0),
    #mu_kappa=0.2,
    #mu_alpha=2,
    #sigmaD=0.1,
    sigmaP=1,
    kappa = runif(max(dataToJags$riverID), 10,30),
    #sigma_alpha=10,
    area=area_inits,
    tq=rep(10,max(dataToJags$riverID)),
    beta=rep(30,max(dataToJags$riverID))
  )
}


#ancien prior = hyperparamètres
#p[year[y]]~dbeta(2,2)
#lambda[y]<-d[year[y]]*S[y]

parameters <-c(
  #"P","p","d",
  "tq","d",
  "s","theta",
  "pCol",
  "kappa","alpha","beta",
  "mu_alpha","mu_kappa","mu_beta",
  "sigma_alpha","sigma_kappa","sigma_beta",
  "Dens_pred","P_pred","Dens_pred_all","P_pred_all",
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
                n.iter = 5000*1,    # 10000 MCMC iterations + si converge pas
                n.burnin = 2000,   # discard first X iterations
                n.thin = 1
                ) # keep every X iterations //ex: garde tous les 100 itérations

save(jagsfit,file="results/jagsfit.Rdata")


## RESULTS ####
# Save densities estimates for each pop and age of colonisation (medians)
DensitiesByPop <- jagsfit$BUGSoutput$median$Dens_pred
colnames(DensitiesByPop) <- 1:57
rownames(DensitiesByPop) <- levels(factor(data$basin))
write.csv(round(DensitiesByPop,3), file="results/DensitiesByPop_median.csv")

pdf(file="results/MCMC.pdf")
#print(jagsfit)
#traplot(jagsfit, parms = c("sigma_eps"))#trois chaines, trois paramètres
#traplot(jagsfit, parms = c("p"))#trois chaines, trois paramètres
#caterplot(jagsfit, parms = c("p"), reorder=FALSE)#trois chaines, trois paramètres
#traplot(jagsfit, parms = c("sigmaP"))#trois chaines, trois paramètres
#traplot(jagsfit, parms = c("muP"))#trois chaines, trois paramètres

traplot(jagsfit, parms = c("sigmaD", "sigmaP" ,"sigma_beta","s"))#trois chaines, trois paramètres
#traplot(jagsfit, parms = c("muD"))#trois chaines, trois paramètres
#caterplot(jagsfit, parms = c("p"), reorder=FALSE)#trois chaines, trois paramètres
#denplot(jagsfit, parms = c("p","d")) #distrib posteriori marginales

denplot(jagsfit, parms = c("s")) #distrib posteriori marginales

traplot(jagsfit, parms = c("pCol")) #distrib posteriori marginales
caterplot(jagsfit,"pCol", reorder = FALSE, horizontal=FALSE)#

traplot(jagsfit, parms = c("delta")) #distrib posteriori marginales
denplot(jagsfit, parms = c("beta")) #distrib posteriori marginales
caterplot(jagsfit, parms = c("delta")) #distrib posteriori marginales
caterplot(jagsfit, parms = c("delta[2]")) #distrib posteriori marginales


traplot(jagsfit, parms = c("mu_kappa")) #distrib posteriori marginales
traplot(jagsfit, parms = c("kappa"));#distrib posteriori marginales
traplot(jagsfit, parms = c("alpha")) #distrib posteriori marginales
traplot(jagsfit, parms = c("mu_beta")) #distrib posteriori marginales
traplot(jagsfit, parms = c("beta")) #distrib posteriori marginales
#traplot(jagsfit, parms = c("kappa","alpha","beta")) #distrib posteriori marginales

caterplot(jagsfit,"kappa", reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));
caterplot(jagsfit,"alpha", reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));
caterplot(jagsfit,"beta", reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));



traplot(jagsfit, parms = c("muS","sigmaS"))#trois chaines, trois paramètres
caterplot(jagsfit,"muS", reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));
caterplot(jagsfit,"area", reorder = FALSE, horizontal=FALSE);points(data$area)

#caterplot(jagsfit, paste0("p[",year_vec,"]"), reorder = FALSE, horizontal=FALSE);#points(0.4)
#caterplot(jagsfit, paste0("muD[",year_vec,"]"), reorder = FALSE, horizontal=FALSE);#points(0.4)
#caterplot(jagsfit,"muP", reorder = FALSE, horizontal=FALSE);#points(0.4)
#caterplot(jagsfit,"muD", reorder = FALSE, horizontal=FALSE);#points(0.4)

#caterplot(jagsfit, "N", reorder = FALSE, horizontal=FALSE);#points(N) #spécifier N
#caterplot(jagsfit,paste0("epsilonD[",1:32,"]"), reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));#points(0.4)

#caterplot(jagsfit,paste0("P_pred[21,",1:32,"]"), reorder = FALSE, horizontal=FALSE);title(levels(factor(data$basin))[21])
caterplot(jagsfit,paste0("P_pred_all[",1:max(dataToJags$popAge),"]"), reorder = FALSE, horizontal=FALSE, labels=1:max(dataToJags$popAge));title("All populations")

caterplot(jagsfit,paste0("epsilonP[",1:33,"]"), reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));#points(0.4)

caterplot(jagsfit,paste0("Dens_pred_all[",1:max(dataToJags$popAge),"]"), reorder = FALSE, horizontal=FALSE, labels=1:max(dataToJags$popAge));title("All populations")

caterplot(jagsfit,paste0("muD[17,",1:(dataToJags$maxPopAge[17]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[17]));title(levels(factor(data$basin))[28])


par(mfrow=c(2,2))
#caterplot(jagsfit,paste0("muD[1,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[pop])
caterplot(jagsfit,paste0("Dens_pred[17,",1:(dataToJags$maxPopAge[17]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[17]));title(levels(factor(data$basin))[17])
caterplot(jagsfit,paste0("Dens_pred[21,",1:(dataToJags$maxPopAge[21]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[21]));title(levels(factor(data$basin))[21])
caterplot(jagsfit,paste0("Dens_pred[22,",1:(dataToJags$maxPopAge[22]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[22]));title(levels(factor(data$basin))[22])
caterplot(jagsfit,paste0("Dens_pred[28,",1:(dataToJags$maxPopAge[28]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[28]));title(levels(factor(data$basin))[28])

#caterplot(jagsfit,paste0("Dens_pred[10,",1:max(dataToJags$popAge),"]"), reorder = FALSE, horizontal=FALSE, labels=1:max(dataToJags$popAge));title(levels(factor(data$basin))[10])
#caterplot(jagsfit,paste0("Dens_pred[27,",1:max(dataToJags$popAge),"]"), reorder = FALSE, horizontal=FALSE, labels=1:max(dataToJags$popAge));title(levels(factor(data$basin))[27])
#caterplot(jagsfit,paste0("Dens_pred[21,",1:max(dataToJags$popAge),"]"), reorder = FALSE, horizontal=FALSE, labels=1:max(dataToJags$popAge));title(levels(factor(data$basin))[21])
#



dev.off()

muD <- (jagsfit$BUGSoutput$sims.list$muD)
pop17 <- muD[,17,1:(dataToJags$maxPopAge[17])] # uniquement pop 17
tmp <- 1:(dataToJags$maxPopAge[17])
medians <- apply(pop17,2,median)
q2.5 <- apply(pop17,2,quantile, probs=0.025)
q97.5 <- apply(pop17,2,quantile, probs=0.975)
q25 <- apply(pop17,2,quantile, probs=0.25)
q75 <- apply(pop17,2,quantile, probs=0.75)
plot(NULL, xlim=c(1,30),ylim=c(0,30))
points(tmp,medians)
segments(tmp,q2.5,tmp,q97.5)
segments(tmp,q25,tmp,q75, lwd=2)


