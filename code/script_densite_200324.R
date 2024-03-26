library(R2jags)
library(mcmcplots)
load.module("glm")
#setwd("C:/Users/gmbrahy/Documents/Modele_densite/data")

##données observées
#1 site 1 date
#data <- list(C1=31,C2=18,S=1335)

data1<-read.table("data/datajakdenyoung.txt",h=T)
data2<-read.table("data/datajakdenadultgaelle.txt",h=T)
data1$year<-as.numeric(substr(data1$date, 7, 10))
data2$year<-as.numeric(substr(data2$date, 7, 10))

#pour data1 et data 2 même nb de colonnes
#data1$site<-data1$XYZ
#Pour ajouter colonne site dans data2
#data2bis <- data2[-c(100,22,1332,1361,1367,1371,1372,1616,1635,1958,1977,1991,2012,2013,2014,2015,2017,2018,2019,2026,2027,2028,2031),]
data2$XYZ<-as.character(substr(data2$XYZ, 6, nchar(data2$XYZ)))

#data1$site <- 1:nrow(data1)
#data2$site <- 1:nrow(data2)

## AGE
data1$age <- 0
data2$age <- 1



#data<-rbind(data1,data2)
data <- data1 # Juveniles only


datanona<-subset(data, !is.na(DL1) & !(is.na(DL2)))#& !(is.na(area)) )

Norvegienne<-subset(datanona,basin=="Norvegienne")
table(Norvegienne$year)
#save(Norvegienne,file="data/Norvegienne.Rdata")

#load("data/Norvegienne.Rdata")
#unique(Norvegienne$site)
#Norvegienne$year-min(Norvegienne$year)+1
mydates = as.POSIXlt(Norvegienne$date)
#Norvegienne$year<-as.numeric(format(as.Date(mydates), "%Y"))
Norvegienne$month<-as.numeric(format(as.Date(mydates), "%m"))
#df$day<-as.numeric(format(as.Date(mydates), "%d"))
Norvegienne$jday<-mydates$yday # julian day

Norvegienne <- subset(Norvegienne, month %in% c(12,1,2,3))

Norvegienne$Year_code <- Norvegienne$year-min(Norvegienne$year)+1

# lagged year: les captures de janvier et février sont attribuées à l'année précédente
Year_code_lagged <- ifelse(Norvegienne$month ==12, Norvegienne$Year_code+1, Norvegienne$Year_code)

#creer object liste qui stocke les données pour jags
datatojags<-list(n=NROW(Norvegienne), 
                 DL1=Norvegienne$DL1,
                 DL2=Norvegienne$DL2,
                 DL3=Norvegienne$DL3,
                 S=as.numeric(Norvegienne$area),
                 AGE = Norvegienne$age,
                 year=Year_code_lagged,#Norvegienne$Year_code,
                 max_year = max(Year_code_lagged)
)

year_vec =unique(Year_code_lagged)#unique(Norvegienne$Year_code)



##2. Modelisation statistique: inférence des paramètres en fonction des données simulées
#langage bugs

modelstat<-function()
{
  #boucle
  for (y in 1:n){ #y boucle sur les lignes du dataframe
    #likelihood
    N[y]~dpois(lambda[y]) #N tiré dans poisson dépend de param lambda
    DL1[y]~dbin(p[y],N[y]) #C1 vecteur. y=1-> premier élemt C1. C1 varie tous les ans
    
    N2[y]<-(N[y]-DL1[y])
    DL2[y]~dbin(p[y],N2[y])
    
    N3[y]<-(N2[y]-DL2[y])
    DL3[y]~dbin(p[y],N3[y])
    
    
    #ancien prior = hyperparamètres
    #p[year[y]]~dbeta(2,2)
    lambda[y]<-d[y]*S[y]
    #d[y]~dgamma(1,1)
    #d[y]~dnorm(muD[year[y]], tauD[year[y]]);T(0,)#gamma défini positif 
    d[y]~dlnorm(log_muD[y], tauD)
    #log_muD[y] <- gamma[1]+gamma[2]*(year[y] - mean(year[]))# (pow(year[y],gamma[2]))#+(pow(year[y],gamma[2+AGE[y]]))
    log_muD[y] <- pow(year[y] - mean(year[]),gamma[2])# (pow(year[y],gamma[2]))#+(pow(year[y],gamma[2+AGE[y]]))
    #p[y]~dbeta(2,2)
    
    logit(p[y]) <- logit_p[y]
    logit_p[y]~dnorm(log_muP[y], tauP)
    log_muP[y] <- delta[1]#+ delta[2]*AGE[y] #+ (pow(year[y],delta[3]))
    
    # likelihood for  the area. NB: not clear trend as a function of time or population age so don't bother. Like wise for river effect. 
    S[y] ~ dlnorm(muS,tauS)
  }#end boucle
  

  #prior 
  #p~dbeta(alpha,beta)
  for (t in 1:max_year){
    #p[t] <- P#~dbeta(alpha,beta)# proba capture /an
    #d[a]~dgamma(1,1)#gamma défini positif
    #muD[a]~dgamma(0.1, 0.1)# densité moyenne/an
    #muD[a]~dnorm(0, 0.1)# densité moyenne/an
    #log_muD[t]<-log(muD[t])
    #log_muD[t] <- gamma[1]+ gamma[2]*t
    #log_muD[t] <- gamma[1]+ (pow(t,gamma[2]))
    #muD[t]<- exp(gamma[1]+gamma[2]*(t - mean(year[])))
    muD[t]<- exp((pow((t - mean(year[])),gamma[2])))

    # tauD[t] <- pow(sigmaD[t],-2)# variance intra-annuelle
    # sigmaD[t] ~ dunif(0,10)
    
    #log_muP[t] <- delta[1]+ delta[2]*AGE[] + (pow(t,delta[3]))
    muP[t,1]<- ilogit(delta[1])#+ (pow(t,delta[3])))
    #muP[t,2]<- ilogit(delta[1]+ delta[2]) #+ (pow(t,delta[3])))
    
    # tauP[t] <- pow(sigmaP[t],-2)# variance intra-annuelle
    # sigmaP[t] ~ dunif(0,10)
  }
  
  alpha~dgamma(0.1, 0.1)
  beta~dgamma(0.1, 0.1)
  gamma[1]~dnorm(0, 0.1)
  gamma[2]~dnorm(0, 0.1)#;T(0,)#~dgamma(1, 1)
  gamma[3]~dnorm(0, 0.1)#;T(0,)#~dgamma(1, 1)
  #delta[1]~dnorm(0, 0.1)
  delta[1]<- ilogit(pmoy)
  pmoy~dbeta(4,2)
  delta[2]~dnorm(0, 0.1)#~dgamma(1, 1)
  delta[3]~dnorm(0, 0.1)
  muS~dnorm(0, 0.1)#~dgamma(1, 1)
  tauS <- pow(sigmaS,-2)# variance intra-annuelle
  sigmaS ~ dunif(0,100)
  tauD <- pow(sigmaD,-2)# variance intra-annuelle
  sigmaD ~ dunif(0,10)
  tauP <- pow(sigmaP,-2)# variance intra-annuelle
  sigmaP ~ dunif(0,10)
  
  #P ~dbeta(alpha,beta)
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

truc<-rep(NA, length(datatojags$S))
truc[is.na(datatojags$S)]<- 250 

inits<-function(){
  list(
    N=(datatojags$DL1+datatojags$DL2+25),
    #p=p,d=d
    #alpha= 10, beta=5,
    #p=rep(0.8,max(datatojags$year)),
    pmoy=0.7,
    delta=c(NA,0,0.3),
    gamma=c(-2,1,0),
    #log_muD=rep(1,max(datatojags$year))
    #p=rbeta(max(datatojags$year),2,2),
    #d=rgamma(max(datatojags$year),1,1)
    #d=rep(0.,max(datatojags$year))
    S=truc
  )
}





#ancien prior = hyperparamètres
#p[year[y]]~dbeta(2,2)
#lambda[y]<-d[year[y]]*S[y]

parameters <-c(
  #"P","p","d",
  "muD","sigmaD",
  "alpha","beta","gamma","delta",
  "muP","sigmaP",
  'muS',"sigmaS",
  "S"
  ) #vecteur
#appeler Jags pour compiler : données, modèle, valeurs initiales
jagsfit <- jags(datatojags,  
                model.file = modelstat,
                parameters.to.save = parameters,  
                n.chains = 2,  # Number of chains to run.
                inits = inits,  # initial values for hyperparameters
                n.iter = 10000*5,    # 10000 MCMC iterations + si converge pas
                n.burnin = 5000*5,   # discard first X iterations
                n.thin = 5) # keep every X iterations //ex: garde tous les 100 itérations


print(jagsfit)

#traplot(jagsfit, parms = c("p"))#trois chaines, trois paramètres
#caterplot(jagsfit, parms = c("p"), reorder=FALSE)#trois chaines, trois paramètres
traplot(jagsfit, parms = c("sigmaP"))#trois chaines, trois paramètres
traplot(jagsfit, parms = c("muP"))#trois chaines, trois paramètres

traplot(jagsfit, parms = c("sigmaD"))#trois chaines, trois paramètres
traplot(jagsfit, parms = c("muD"))#trois chaines, trois paramètres
#caterplot(jagsfit, parms = c("p"), reorder=FALSE)#trois chaines, trois paramètres
#denplot(jagsfit, parms = c("p","d")) #distrib posteriori marginales
traplot(jagsfit, parms = c("gamma","delta")) #distrib posteriori marginales
denplot(jagsfit, parms = c("gamma","delta")) #distrib posteriori marginales
caterplot(jagsfit, parms = c("alpha","beta","gamma","delta")) #distrib posteriori marginales

traplot(jagsfit, parms = c("muS","sigmaS"))#trois chaines, trois paramètres

#caterplot(jagsfit, paste0("p[",year_vec,"]"), reorder = FALSE, horizontal=FALSE);#points(0.4)
#caterplot(jagsfit, paste0("muD[",year_vec,"]"), reorder = FALSE, horizontal=FALSE);#points(0.4)
caterplot(jagsfit,"muP", reorder = FALSE, horizontal=FALSE);#points(0.4)
caterplot(jagsfit,"muD", reorder = FALSE, horizontal=FALSE);#points(0.4)
caterplot(jagsfit,"S", reorder = FALSE, horizontal=FALSE);#points(0.4)
#caterplot(jagsfit, "N", reorder = FALSE, horizontal=FALSE);#points(N) #spécifier N


