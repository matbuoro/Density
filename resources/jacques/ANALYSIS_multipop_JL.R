## Not run: 
## remove (almost) everything in the working environment.
## You will get no warning, so don't do this unless you are really sure.
rm(list = ls())
setwd( "C:/Dropbox/travail/Kerguelen/densité/git") # ton chemin.
# PACKAGES ####
library(R2jags)
library(mcmcplots)
load.module("glm")
#setwd("C:/Users/gmbrahy/Documents/Modele_densite/data")

## DATA ####
#source("code/DATA_format.R")
source("code/DATA_format.R")
str(dataToJags)

## MODEL ####
##2. Modelisation statistique: inférence des paramètres en fonction des données 
#langage bugs
#source("code/MODEL_density_log.R")
#source("code/MODEL_density_log_simple_working.R")
source("jacques/MODEL_density_log_kappa_beta_working.R")
#source("code/MODEL_density_log_random_working.R")
#source("code/MODEL_density_BH_jak.R")



## tracking parameters
parameters <-c(
  #"N",
  #"P","p","d",
  #"tq","d","dens",
  "s","theta",
  #"pCol",
  #"k_prior","D",
  "kappa","alpha","beta",
  "mu_alpha","mu_kappa","mu_kappa_bis","mu_beta",
  #"Dens_pred","P_pred","Dens_pred_all","P_pred_all",
  "Dens_pred",
  #"muD","sigmaD",
  #"gamma","delta",
  #"muP",
  "sigmaP",
  'muS',"sigmaS",
  "area",#"sigma_eps",
  "epsilonD","epsilonP",
  "tau_alpha","tau_beta","tau_kappa",
  "tau_epsilon","tau_epsilonD",
  "tauD"
  ) #vecteur



## INITS ####
area_inits<-rep(NA, length(dataToJags$area))
area_inits[is.na(dataToJags$area)]<- 250 

N_inits=NULL
for (i in 1:length(dataToJags$DL1)){
N_inits[i] <- ceiling(sum(c(dataToJags$DL1[i],dataToJags$DL2[i],dataToJags$DL3[i]
  ,dataToJags$P1[i],dataToJags$P2[i],dataToJags$PE[i]
  ), na.rm = TRUE)/0.7)
  #), na.rm = TRUE)/)  
}
#N_inits[1365]<-1
#N_inits[958]<-40
#N_inits[260]<-22
#N_inits[133]<-150
inits<-function(){ # works for logistic.
  list(
    N=N_inits,
    pmoy=0.7,
    #delta=c(NA,0),#0.3),
    mu_kappa=30,
    mu_alpha=4,
    mu_beta=0.15,
    sigmaP=1,
    k_prior=0.5, D = 0.99,
    sigma_alpha= 0.3,
    sigma_beta=0.005,
    sigma_kappa=0.7,
    kappa = runif(max(dataToJags$riverID), 13,23),
    alpha = runif(max(dataToJags$riverID), 2,12),
    beta = runif(max(dataToJags$riverID), 0.02,0.4),
    #log_muD = runif(1426, 1, 23),
    area=area_inits,
    tq=rep(10,max(dataToJags$riverID))#,
    #beta=rep(0,max(dataToJags$riverID))
  )
}
inits<-function(){ # works for BH.
  list(
    N=N_inits,
    pmoy=0.7,
    delta=c(NA,0),#0.3),
    mu_kappa=25,
    mu_alpha=1,
    mu_beta=1.15,
    sigmaP=1,
    k_prior=0.5, D = 0.99,
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

## straight
jagsfit <- jags(dataToJags,  
                model.file = modelstat,
                parameters.to.save = parameters,  
                n.chains = 2,  # Number of chains to run.
                inits = inits,  # initial values for hyperparameters
                n.iter = 5000*1,   #MCMC iterations, ajouter si converge pas
                n.burnin = 2000,   # discard first X iterations
                n.thin = 1
                ) # keep every X iterations //ex: garde tous les 100 itérations

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

save(jagsfit,file="results/jagsfitBHFULL.Rdata")


#update(jagsfit, 50000)

## RESULTS ####
# Save densities estimates for each pop and age of colonisation (medians)
DensitiesByPop <- jagsfit$BUGSoutput$median$Dens_pred
colnames(DensitiesByPop) <- 1:51
rownames(DensitiesByPop) <- levels(factor(data$basin))
write.csv(round(DensitiesByPop,3), file="results/DensitiesByPop_median.csv")

pdf(file="results/MCMC.pdf")
#print(jagsfit)
#traplot(jagsfit, parms = c("sigma_eps"))#trois chaines, trois paramètres
#traplot(jagsfit, parms = c("p"))#trois chaines, trois paramètres
#caterplot(jagsfit, parms = c("p"), reorder=FALSE)#trois chaines, trois paramètres
#traplot(jagsfit, parms = c("sigmaP"))#trois chaines, trois paramètres
#traplot(jagsfit, parms = c("muP"))#trois chaines, trois paramètres
# 
# traplot(jagsfit, parms = c("sigmaD", "sigmaP" ,"sigma_beta","s"))#trois chaines, trois paramètres
# #traplot(jagsfit, parms = c("muD"))#trois chaines, trois paramètres
# #caterplot(jagsfit, parms = c("p"), reorder=FALSE)#trois chaines, trois paramètres
# #denplot(jagsfit, parms = c("p","d")) #distrib posteriori marginales
# 
# denplot(jagsfit, parms = c("s")) #distrib posteriori marginales
# 
# traplot(jagsfit, parms = c("pCol")) #distrib posteriori marginales
# caterplot(jagsfit,"pCol", reorder = FALSE, horizontal=FALSE)#
# 
# 
# traplot(jagsfit, parms = c("k_prior"))
# traplot(jagsfit, parms = c("D")) 
# 
# traplot(jagsfit, parms = c("delta")) 
# traplot(jagsfit, parms = c("beta")) 
# caterplot(jagsfit, parms = c("delta")) 
# caterplot(jagsfit, parms = c("delta[2]")) 
# traplot(jagsfit, parms = c("gamma")) 
# 
# #estimation naive densite
# traplot(jagsfit, parms = c("muD[1]"))
# 
# traplot(jagsfit, parms = c("mu_kappa_bis")) #comparaison avec paramètre aléatoire
# traplot(jagsfit, parms = c("kappa"));
# traplot(jagsfit, parms = c("alpha"))
traplot(jagsfit, parms = c("mu_beta")) 
traplot(jagsfit, parms = c("mu_alpha")) 
traplot(jagsfit, parms = c("mu_kappa")) 
# traplot(jagsfit, parms = c("tauD")) 
# traplot(jagsfit, parms = c("tau_epsilon")) 
# #traplot(jagsfit, parms = c("muD")) 
# traplot(jagsfit, parms = c("beta")) #distrib posteriori marginales
# #traplot(jagsfit, parms = c("kappa","alpha","beta")) #distrib posteriori marginales
# 
# traplot(jagsfit, parms = c("tau_alpha")) 
# traplot(jagsfit, parms = c("tau_beta")) 
# traplot(jagsfit, parms = c("tau_kappa")) 
# 
# caterplot(jagsfit,"kappa", reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));
# caterplot(jagsfit,"beta", reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));
# caterplot(jagsfit,"alpha", reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));
# traplot(jagsfit, parms = c("beta[12]")) 
# 
# caterplot(jagsfit,"muD", reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));
# # caterplot(jagsfit,paste0("muD[",700:722,"]"), reorder = FALSE, horizontal=FALSE); title("Densites naives")
# 
# 
# 
# traplot(jagsfit, parms = c("muS","sigmaS"))#trois chaines, trois paramètres
# caterplot(jagsfit,"muS", reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));
# caterplot(jagsfit,"area", reorder = FALSE, horizontal=FALSE);points(data$area)
# 
# #caterplot(jagsfit, paste0("p[",year_vec,"]"), reorder = FALSE, horizontal=FALSE);#points(0.4)
# #caterplot(jagsfit, paste0("muD[",year_vec,"]"), reorder = FALSE, horizontal=FALSE);#points(0.4)
# #caterplot(jagsfit,"muP", reorder = FALSE, horizontal=FALSE);#points(0.4)
# #caterplot(jagsfit,"muD", reorder = FALSE, horizontal=FALSE);#points(0.4)
# 
# #caterplot(jagsfit, "N", reorder = FALSE, horizontal=FALSE);#points(N) #spécifier N
# #caterplot(jagsfit,paste0("epsilonD[",1:32,"]"), reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));#points(0.4)
# 
# caterplot(jagsfit,paste0("P_pred[",1:22,"]"), reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin))); title("Probabilités de capture")
# 
# caterplot(jagsfit,paste0("P_pred[28,",1:32,"]"), reorder = FALSE, horizontal=FALSE);title(levels(factor(data$basin))[28])
# caterplot(jagsfit,paste0("P_pred_all[",1:max(dataToJags$popAge),"]"), reorder = FALSE, horizontal=FALSE, labels=1:max(dataToJags$popAge));title("All populations")
# 
# caterplot(jagsfit,paste0("epsilonP[",1:50,"]"), reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));#points(0.4)
# caterplot(jagsfit,paste0("epsilonD[",1:33,"]"), reorder = FALSE, horizontal=FALSE, labels=levels(factor(data$basin)));#points(0.4)
# 
# caterplot(jagsfit,paste0("Dens_pred_all[",1:max(dataToJags$popAge),"]"), reorder = FALSE, horizontal=FALSE, labels=1:max(dataToJags$popAge));title("All populations")
# 
# caterplot(jagsfit,paste0("muD[17,",1:(dataToJags$maxPopAge[17]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[17]));title(levels(factor(data$basin))[28])
# 
# 
# ###very handy to brows through all populations predictions. Use click, forward only atm.
# for (i in 1:length(trueMaxPopAge)){
#     a<-paste0("Dens_pred[" ,  as.character(i) , ","  , 1:(dataToJags$trueMaxPopAge[i]) ,  "]"  )
#     caterplot(jagsfit, a ,  reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$trueMaxPopAge[i]));title(levels(factor(data$basin))[i])
#     locator(1)
# }
# 
# 
# #caterplot(jagsfit,paste0("muD[1,",1:60,"]"), reorder = FALSE, horizontal=FALSE, labels=1:60);title(levels(factor(data$basin))[pop])
# caterplot(jagsfit,paste0("Dens_pred[1,",1:(dataToJags$maxPopAge[1]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[1]));title(levels(factor(data$basin))[1])
# caterplot(jagsfit,paste0("Dens_pred[2,",1:(dataToJags$maxPopAge[2]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[2]));title(levels(factor(data$basin))[2])
# caterplot(jagsfit,paste0("Dens_pred[3,",1:(dataToJags$maxPopAge[3]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[3]));title(levels(factor(data$basin))[3])
# caterplot(jagsfit,paste0("Dens_pred[4,",1:(dataToJags$maxPopAge[4]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[4]));title(levels(factor(data$basin))[4])
# caterplot(jagsfit,paste0("Dens_pred[5,",1:(dataToJags$maxPopAge[5]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[5]));title(levels(factor(data$basin))[5])
# caterplot(jagsfit,paste0("Dens_pred[6,",1:(dataToJags$maxPopAge[6]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[6]));title(levels(factor(data$basin))[6])
# caterplot(jagsfit,paste0("Dens_pred[8,",1:(dataToJags$maxPopAge[8]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[8]));title(levels(factor(data$basin))[8])
# 
# 
# caterplot(jagsfit,paste0("Dens_pred[9,",1:(dataToJags$maxPopAge[9]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[9]));title(levels(factor(data$basin))[9])
# 
# caterplot(jagsfit,paste0("Dens_pred[10,",1:(dataToJags$maxPopAge[10]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[10]));title(levels(factor(data$basin))[10])
# caterplot(jagsfit,paste0("Dens_pred[11,",1:(dataToJags$maxPopAge[11]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[11]));title(levels(factor(data$basin))[11])
# caterplot(jagsfit,paste0("Dens_pred[12,",1:(dataToJags$maxPopAge[12]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[12]));title(levels(factor(data$basin))[12])
# caterplot(jagsfit,paste0("Dens_pred[13,",1:(dataToJags$maxPopAge[13]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[13]));title(levels(factor(data$basin))[13])
# caterplot(jagsfit,paste0("Dens_pred[14,",1:(dataToJags$maxPopAge[14]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[14]));title(levels(factor(data$basin))[14])
# caterplot(jagsfit,paste0("Dens_pred[15,",1:(dataToJags$maxPopAge[15]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[15]));title(levels(factor(data$basin))[15])
# 
# 
# caterplot(jagsfit,paste0("Dens_pred[16,",1:(dataToJags$maxPopAge[16]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[16]));title(levels(factor(data$basin))[16])
# 
# caterplot(jagsfit,paste0("Dens_pred[17,",1:(dataToJags$maxPopAge[17]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[17]));title(levels(factor(data$basin))[17])
# 
# caterplot(jagsfit,paste0("Dens_pred[18,",1:(dataToJags$maxPopAge[18]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[18]));title(levels(factor(data$basin))[18])
# 
# caterplot(jagsfit,paste0("Dens_pred[20,",1:(dataToJags$maxPopAge[20]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[20]));title(levels(factor(data$basin))[20])
# 
# caterplot(jagsfit,paste0("Dens_pred[30,",1:(dataToJags$maxPopAge[30]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[30]));title(levels(factor(data$basin))[30])
# caterplot(jagsfit,paste0("Dens_pred[31,",1:(dataToJags$maxPopAge[31]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[31]));title(levels(factor(data$basin))[31])
# caterplot(jagsfit,paste0("Dens_pred[32,",1:(dataToJags$maxPopAge[32]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[32]));title(levels(factor(data$basin))[32])
# caterplot(jagsfit,paste0("Dens_pred[33,",1:(dataToJags$maxPopAge[33]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[33]));title(levels(factor(data$basin))[33])
# caterplot(jagsfit,paste0("Dens_pred[34,",1:(dataToJags$maxPopAge[34]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[34]));title(levels(factor(data$basin))[34])
# 
# 
# caterplot(jagsfit,paste0("Dens_pred[40,",1:(dataToJags$maxPopAge[40]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[40]));title(levels(factor(data$basin))[40])
# caterplot(jagsfit,paste0("Dens_pred[42,",1:(dataToJags$maxPopAge[42]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[42]));title(levels(factor(data$basin))[42])
# caterplot(jagsfit,paste0("Dens_pred[43,",1:(dataToJags$maxPopAge[43]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[43]));title(levels(factor(data$basin))[43])
# 
# caterplot(jagsfit,paste0("Dens_pred[45,",1:(dataToJags$maxPopAge[45]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[45]));title(levels(factor(data$basin))[45])
# caterplot(jagsfit,paste0("Dens_pred[3,",1:(dataToJags$maxPopAge[3]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[3]));title(levels(factor(data$basin))[3])
# 
# 
# caterplot(jagsfit,paste0("Dens_pred[22,",1:(dataToJags$maxPopAge[22]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[22]));title(levels(factor(data$basin))[22])
# caterplot(jagsfit,paste0("Dens_pred[19,",1:(dataToJags$maxPopAge[19]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[19]));title(levels(factor(data$basin))[19])
# caterplot(jagsfit,paste0("Dens_pred[18,",1:(dataToJags$maxPopAge[15]),"]"), reorder = FALSE, horizontal=FALSE, labels=1:(dataToJags$maxPopAge[15]));title(levels(factor(data$basin))[15])
# 
# #caterplot(jagsfit,paste0("Dens_pred[10,",1:max(dataToJags$popAge),"]"), reorder = FALSE, horizontal=FALSE, labels=1:max(dataToJags$popAge));title(levels(factor(data$basin))[10])
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
# 




# muD <- (jagsfit$BUGSoutput$sims.list$muD)
# pop <- 28
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
dens <- (jagsfit$BUGSoutput$sims.list$dens)
for (pop in unique(dataToJags$riverID)){
#pop <- 10
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





## personal brew for inits creation. unelegant yet working.

load(file="results/futureInits3.Rdata")
futureInits<-jagsfit$BUGSoutput
inits<-function(){
  list(
    N=N_inits,
    alpha=futureInits$last.values[[1]]$alpha,
    beta=futureInits$last.values[[1]]$beta,
    kappa=futureInits$last.values[[1]]$kappa,
    pmoy=0.7,
    delta=c(NA,0),#0.3),
    mu_kappa=futureInits$last.values[[1]]$mu_kappa,
    mu_alpha=futureInits$last.values[[1]]$mu_alpha,
    mu_beta=futureInits$last.values[[1]]$mu_beta,
    sigmaP=1,
    k_prior=0.5, D = 0.99,
    #kappa = runif(max(dataToJags$riverID), 21,49),
    #alpha = runif(max(dataToJags$riverID), 5,39),
    #beta = runif(max(dataToJags$riverID), 0.02,0.4),
   # log_muD = runif(1426, 1, 23),
    area=area_inits,
    tq=rep(10,max(dataToJags$riverID))#,
    #beta=rep(0,max(dataToJags$riverID))  
  )
}
## JAGS ####
#appeler Jags pour compiler : données, modèle, valeurs initiales

## experimental
jagsfit <- jags(dataToJags,  
                model.file = modelstat,
                parameters.to.save = parameters,  
                n.chains = 2,  # Number of chains to run.
                inits = inits,  # initial values for hyperparameters
                n.iter = 100*1,   #MCMC iterations, ajouter si converge pas
                n.burnin = 20,   # discard first X iterations
                n.thin = 1
                ) # keep every X iterations //ex: garde tous les 100 itérations

save(jagsfit,file="results/jagsfit.Rdata")
#futureInits<-jagsfit$BUGSoutput
save(futureInits,file="results/futureInits.Rdata")
save(jagsfit,file="results/futureInits3.Rdata")






futureInits<-jagsfit$BUGSoutput
N_inits_brewed<-futureInits$last.values[[1]]$N
inits<-function(){
  list(
    N=N_inits_brewed,
    alpha=futureInits$last.values[[1]]$alpha,
    beta=futureInits$last.values[[1]]$beta,
    kappa=futureInits$last.values[[1]]$kappa,
    pmoy=0.7,
    delta=c(NA,0),#0.3),
    mu_kappa=futureInits$last.values[[1]]$mu_kappa,
    mu_alpha=futureInits$last.values[[1]]$mu_alpha,
    mu_beta=futureInits$last.values[[1]]$mu_beta,
    sigmaP=1,
    k_prior=0.5, D = 0.99,
    #kappa = runif(max(dataToJags$riverID), 21,49),
    #alpha = runif(max(dataToJags$riverID), 5,39),
    #beta = runif(max(dataToJags$riverID), 0.02,0.4),
   # log_muD = runif(1426, 1, 23),
    area=area_inits,
    tq=rep(10,max(dataToJags$riverID))#,
    #beta=rep(0,max(dataToJags$riverID))  
  )
}


