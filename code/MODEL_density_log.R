modelstat<-function(){
  
 ## DE LURY
  for (j in n1[1]:n1[2]){ #y boucle sur les lignes du dataframe
    # Abundance
    N[j]~dpois(lambda[j]) #N tiré dans poisson dépend de param lambda
    #N[j] ~ dnegbin(q[j], r)
    #q[j] <- r / (r + lambda[j])  # Convert lambda to p[j]

    # De Lury method
    DL1[j]~dbin(p1[j],N[j]) #C1 vecteur. y=1-> premier élemt C1. C1 varie tous les ans
    N2[j]<-(N[j]-DL1[j])
    DL2[j]~dbin(p1[j],N2[j])
    
    # Density
    lambda[j]<-(dens[j]*(area[j]/100)) # number of fish / 100m2
    dens[j]~dlnorm(log(muD[j]), tauD)
    #muD[j] <- alpha_muD[riverID[j], year[j]]

    ##logistic
    #muD[j] <- kappa[riverID[j]] / (1+alpha[riverID[j]] * exp(beta[riverID[j]]*(year_capture[j])))
    #muD[j] <- kappa[riverID[j]] / (1+mu_alpha * exp(-beta[riverID[j]]*(year[j]))) #log
    muD[j] <- kappa / (1+alpha * exp(-beta*(popAge[j]))) #log
    #muD[j] <- kappa[riverID[j]] / (1+alpha * exp(-beta*(popAge[j]))) #log
    

    # Capture probability
    #logit(p1[j]) <- delta + epsilonP[riverID[j]]
    logit(p1[j]) <- logit_p1[j]
    logit_p1[j]~dnorm(log_muP1[j], tauP)
    #log_muP1[j] <- delta + epsilonP[riverID[j]]# /!\ we consider year effect instead of age because protocol and sampling effort could have change over time.
    log_muP1[j] <- delta + epsilonP[riverID[j]]
  }#end boucle


  # PETERSEN
  for (j in n2[1]:n2[2]){ #y boucle sur les lignes du dataframe
    # Abundance
    N[j]~dpois(lambda[j]) #N tiré dans poisson dépend de param lambda
    #N[j] ~ dnegbin(q[j], r)
    #q[j] <- r / (r + lambda[j])  # Convert lambda to p[j]

    # Petersen method
    P1[j]~dbin(p2[j],N[j])
    P2[j]~dbin(p2[j],N[j])
    
    # Density
    lambda[j]<-(dens[j]*(area[j]/100)) # number of fish / 100m2
    dens[j]~dlnorm(log(muD[j]), tauD)
    #muD[j] <- alpha_muD[riverID[j], year[j]]

    ##logistic
    #muD[j] <- kappa[riverID[j]] / (1+alpha[riverID[j]] * exp(beta[riverID[j]]*(year_capture[j])))
    #muD[j] <- kappa[riverID[j]] / (1+mu_alpha * exp(-beta[riverID[j]]*(year[j]))) #log
    muD[j] <- kappa / (1+alpha * exp(-beta*(popAge[j]))) #log
    #muD[j] <- kappa[riverID[j]] / (1+alpha * exp(-beta*(popAge[j]))) #log
    
    # Capture probability
    #logit(p2[j]) <- delta + epsilonP[riverID[j]]
    logit(p2[j]) <- logit_p2[j]
    logit_p2[j]~dnorm(log_muP2[j], tauP)
    #log_muP2[j] <- delta + epsilonP[riverID[j]]# /!\ we consider year effect instead of age because protocol and sampling effort could have change over time.
    log_muP2[j] <- delta + epsilonP[riverID[j]]
  }#end boucle


  # PE
  for (j in n3[1]:n3[2]){ #y boucle sur les lignes du dataframe
    # Abundance
    N[j]~dpois(lambda[j]) #N tiré dans poisson dépend de param lambda
    # N[j] ~ dnegbin(q[j], r)
    # q[j] <- r / (r + lambda[j])  # Convert lambda to p[j]

    # PE only
    PE[j]~dbin(p3[j],N[j]) #C1 vecteur. y=1-> premier élemt C1. C1 varie tous les ans
    
    # Density
    lambda[j]<-(dens[j]*(area[j]/100)) # number of fish / 100m2
    dens[j]~dlnorm(log(muD[j]), tauD)
    #muD[j] <- alpha_muD[riverID[j], year[j]]

    ##logistic
    #muD[j] <- kappa[riverID[j]] / (1+alpha[riverID[j]] * exp(beta[riverID[j]]*(year_capture[j])))
    #muD[j] <- kappa[riverID[j]] / (1+mu_alpha * exp(-beta[riverID[j]]*(year[j]))) #log
    muD[j] <- kappa / (1+alpha * exp(-beta*(popAge[j]))) #log
    #muD[j] <- kappa[riverID[j]] / (1+alpha * exp(-beta*(popAge[j]))) #log

    # Capture probability
    #logit(p3[j]) <- delta + epsilonP[riverID[j]]
    logit(p3[j]) <- logit_p3[j]
    logit_p3[j]~dnorm(log_muP3[j], tauP)
    log_muP3[j] <- delta + epsilonP[riverID[j]]# /!\ we consider year effect instead of age because protocol and sampling effort could have change over time.
    #log_muP3[j] <- delta[3] #+ epsilonP[riverID[j]]
  }#end boucle
  
  for (j in 1:n){
  area[j] ~ dlnorm(muS[riverID[j]],tauS);T(,2500)
  }
  
  
  ##PRIOR
  for (i in 1:max(riverID)){
    muS[i]~dnorm(0, 0.01)#~dgamma(1, 1)
    epsilonP[i]~dnorm(0,tau_epsilon) 

    #naive estimation
    # for (y in 1:maxMetapopAge){
    #   alpha_muD[i, y] ~ dlnorm(0, 1)
    # }
  }

  
  kappa ~ dlnorm(3.1, 1/(0.69*0.69)) # to keep 95% of kappa between [10; 50]
  #tau_kappa <- pow(sigma_kappa,-2)
  #sigma_kappa ~ dunif(0,2)
  #mu_kappa ~ dlnorm(3.1, 1/(0.41*0.41)) # to keep 95% of kappa between [10; 50]
  
  beta~dnorm(0,0.1);T(0,)
  alpha~dlnorm(0,1)
  #alpha~dunif(1,40) 
  
  t50 <- log(alpha)/beta # time to reach 50% of carrying capacity
  
  delta~dnorm(0, 1.5)
  pmoy<-ilogit(delta)
  
  #r ~ dgamma(1, 1)  # Prior for overdispersion
  
  tau_epsilon <- pow(sigma_eps,-2)
  sigma_eps ~ dunif(0,10)


  tauS <- pow(sigmaS,-2)# variance intra-annuelle
  sigmaS ~ dunif(0,1000)
  tauD <- pow(sigmaD,-2)# variance intra-annuelle
  sigmaD ~ dunif(0,1000)
  tauP <- pow(sigmaP,-2)
  sigmaP ~ dunif(0,10)

  ## Prediction
  for (pop in 1:max(riverID)){   
    #P_pred[pop]<- ilogit(delta[censusType[pop]]+ epsilonP[pop])#+ (pow(t,delta[3])))
    # P_DL_pred[pop]<- ilogit(delta[1]+ epsilonP[pop])
    P_pred[pop]<- ilogit(delta + epsilonP[pop])
    # P_PE_pred[pop]<- ilogit(delta[3]+ epsilonP[pop])
  } # end loop pop
  
  for (t in 1:50){
    muD_pred[t] <- kappa / (1+alpha * exp(-beta*(t)))
  }
} #end model

