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
source("code/format_data.R")

# MODEL ####
##2. Modelisation statistique: inférence des paramètres en fonction des données simulées
#langage bugs

modelstat<-function()
{
  #boucle
  for (j in 1:n){ #y boucle sur les lignes du dataframe
    # LIKELIHOOD
    N[j]~dpois(lambda[j]) #N tiré dans poisson dépend de param lambda
    DL1[j]~dbin(p[j],N[j]) #C1 vecteur. y=1-> premier élemt C1. C1 varie tous les ans
    
    N2[j]<-(N[j]-DL1[j])
    DL2[j]~dbin(p[j],N2[j])
    
    #N3[j]<-(N2[j]-DL2[j])
    #DL3[j]~dbin(p[j],N3[j])
    
    
    #ancien prior = hyperparamètres
    #p[year[j]]~dbeta(2,2)
    lambda[j]<-d[j]*area[j]
    #d[j]~dgamma(1,1)
    #d[j]~dnorm(muD[year[j]], tauD[year[j]]);T(0,)#gamma défini positif 
    d[j]~dlnorm(log_muD[j], tauD)
    log_muD[j] <- gamma[1]+gamma[2]*(popAge[j] - mean(popAge[])) + epsilon[1,riverID[j]] # (pow(year[j],gamma[2]))#+(pow(year[j],gamma[2+AGE[j]]))
    #log_muD[j] <- pow(year[j] - mean(year[]),gamma[2])# (pow(year[j],gamma[2]))#+(pow(year[j],gamma[2+AGE[j]]))
    #p[j]~dbeta(2,2)
    
    logit(p[j]) <- logit_p[j]
    logit_p[j]~dnorm(log_muP[j], tauP)
    log_muP[j] <- delta[1]+ delta[2]*year[j] + epsilon[2,riverID[j]]
    
    # likelihood for  the area. NB: not clear trend as a function of time or population age so don't bother. Like wise for river effect. 
    area[j] ~ dlnorm(muS,tauS)
  }#end boucle
  

# PRIOR
  for (i in 1:Nriver){
    epsilon[1,i]~dnorm(0,tau_epsilon[1]) #random effect for density
    epsilon[2,i]~dnorm(0,tau_epsilon[2]) #random effect for capture probability
  }
  tau_epsilon[1] <- pow(sigma_eps[1],-2)# variance intra-annuelle
  sigma_eps[1] ~ dunif(0,10)
  tau_epsilon[2] <- pow(sigma_eps[2],-2)# variance intra-annuelle
  sigma_eps[2] ~ dunif(0,10)

    #alpha~dgamma(0.1, 0.1)
  #beta~dgamma(0.1, 0.1)
  gamma[1]~dnorm(0, 0.1)
  gamma[2]~dnorm(0, 0.1)#;T(0,)#~dgamma(1, 1)
  #gamma[3]~dnorm(0, 0.1)#;T(0,)#~dgamma(1, 1)
  #delta[1]~dnorm(0, 0.1)
  delta[1]<- ilogit(pmoy)
  pmoy~dbeta(4,2)
  delta[2]~dnorm(0, 0.1)#~dgamma(1, 1)
  #delta[3]~dnorm(0, 0.1)
  muS~dnorm(0, 0.1)#~dgamma(1, 1)
  tauS <- pow(sigmaS,-2)# variance intra-annuelle
  sigmaS ~ dunif(0,100)
  tauD <- pow(sigmaD,-2)# variance intra-annuelle
  sigmaD ~ dunif(0,10)
  tauP <- pow(sigmaP,-2)# variance intra-annuelle
  sigmaP ~ dunif(0,10)
  
  #P ~dbeta(alpha,beta)

 # PREDICTION
  for (pop in 1:Nriver){
    for (t in 1:max(popAge)){

    muD[pop,t]<- exp(gamma[1]+gamma[2]*(t - mean(popAge[])) + epsilon[1,pop])

    muP[pop,t]<- ilogit(delta[1]+ delta[2]*t + epsilon[2,pop])#+ (pow(t,delta[3])))
    #muP[t,2]<- ilogit(delta[1]+ delta[2]) #+ (pow(t,delta[3])))

  } # end loop t
  } # end loop pop
}#end model


#hist(rbeta(100000,3,2))


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
  "muD","sigmaD",
  #"alpha","beta",
  "gamma","delta",
  "muP","sigmaP",
  'muS',"sigmaS",
  "area","sigma_eps",
  "epsilon"
  ) #vecteur
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

# Save densities estimates for each pop and age of colonisation (medians)
DensitiesByPop <- jagsfit$BUGSoutput$median$muD
colnames(DensitiesByPop) <- 1:60
rownames(DensitiesByPop) <- levels(factor(data$basin))
write.csv(round(DensitiesByPop,3), file="results/DensitiesByPop_median.csv")

pdf(file="results/MCMC.pdf")
#print(jagsfit)
traplot(jagsfit, parms = c("sigma_eps"))#trois chaines, trois paramètres
#traplot(jagsfit, parms = c("p"))#trois chaines, trois paramètres
#caterplot(jagsfit, parms = c("p"), reorder=FALSE)#trois chaines, trois paramètres
traplot(jagsfit, parms = c("sigmaP"))#trois chaines, trois paramètres
#traplot(jagsfit, parms = c("muP"))#trois chaines, trois paramètres

traplot(jagsfit, parms = c("sigmaD"))#trois chaines, trois paramètres
#traplot(jagsfit, parms = c("muD"))#trois chaines, trois paramètres
#caterplot(jagsfit, parms = c("p"), reorder=FALSE)#trois chaines, trois paramètres
#denplot(jagsfit, parms = c("p","d")) #distrib posteriori marginales
traplot(jagsfit, parms = c("gamma","delta")) #distrib posteriori marginales
denplot(jagsfit, parms = c("gamma","delta")) #distrib posteriori marginales
caterplot(jagsfit, parms = c("gamma","delta")) #distrib posteriori marginales

traplot(jagsfit, parms = c("muS","sigmaS"))#trois chaines, trois paramètres
caterplot(jagsfit,"area", reorder = FALSE, horizontal=FALSE);#points(0.4)

#caterplot(jagsfit, paste0("p[",year_vec,"]"), reorder = FALSE, horizontal=FALSE);#points(0.4)
#caterplot(jagsfit, paste0("muD[",year_vec,"]"), reorder = FALSE, horizontal=FALSE);#points(0.4)
#caterplot(jagsfit,"muP", reorder = FALSE, horizontal=FALSE);#points(0.4)
#caterplot(jagsfit,"muD", reorder = FALSE, horizontal=FALSE);#points(0.4)

#caterplot(jagsfit, "N", reorder = FALSE, horizontal=FALSE);#points(N) #spécifier N
caterplot(jagsfit,paste0("epsilon[1,",1:32,"]"), reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));#points(0.4)
caterplot(jagsfit,paste0("epsilon[2,",1:32,"]"), reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));#points(0.4)



#caterplot(jagsfit,paste0("muD[1,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[pop])
caterplot(jagsfit,paste0("muD[1,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[1])
caterplot(jagsfit,paste0("muD[2,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[2])
caterplot(jagsfit,paste0("muD[3,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[3])
caterplot(jagsfit,paste0("muD[4,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[4])
caterplot(jagsfit,paste0("muD[5,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[5])
caterplot(jagsfit,paste0("muD[6,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[6])
caterplot(jagsfit,paste0("muD[7,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[7])
caterplot(jagsfit,paste0("muD[8,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[8])
caterplot(jagsfit,paste0("muD[9,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[9])
caterplot(jagsfit,paste0("muD[10,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[10])

caterplot(jagsfit,paste0("muD[11,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[11])
caterplot(jagsfit,paste0("muD[12,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[12])
caterplot(jagsfit,paste0("muD[13,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[13])
caterplot(jagsfit,paste0("muD[14,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[14])
caterplot(jagsfit,paste0("muD[15,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[15])
caterplot(jagsfit,paste0("muD[16,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[16])
caterplot(jagsfit,paste0("muD[17,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[17])
caterplot(jagsfit,paste0("muD[18,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[18])
caterplot(jagsfit,paste0("muD[19,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[19])
caterplot(jagsfit,paste0("muD[20,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[20])

dev.off()
