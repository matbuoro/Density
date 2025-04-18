modelstat<-function(){
  
  ## DE LURY
  for (j in n1[1]:n1[2]){
    #for (j in 1:n){ #y boucle sur les lignes du dataframe
    # Abundance
    N[j]~dpois(lambda[j]) #N tiré dans poisson dépend de param lambda
    #N[j] ~ dnegbin(q[j], r)
    #q[j] <- r / (r + lambda[j])  # Convert lambda to p[riverID[j]]
    
    # Density
    lambda[j]<-(dens[j]*(area[j]/100)) # number of fish / 100m2
    dens[j]~dlnorm(log(muD[j]), tauD)
    #muD[j] <- alpha_muD[riverID[j], year[j]]
    
    ##logistic
    #muD[j] <- kappa[riverID[j]] / (1+alpha[riverID[j]] * exp(beta[riverID[j]]*(year_capture[j])))
    #muD[j] <- kappa[riverID[j]] / (1+mu_alpha * exp(-beta[riverID[j]]*(year[j]))) #log
    muD[j] <- kappa / (1+alpha * exp(-beta*(popAge[j]))) #log
    #muD[j] <- kappa[riverID[j]] / (1+alpha * exp(-beta*(popAge[j]))) #log
    
    # De Lury method
    DL1[j]~dbin(p[riverID[j]],N[j]) #C1 vecteur. y=1-> premier élemt C1. C1 varie tous les ans
    N2[j]<-(N[j]-DL1[j])
    DL2[j]~dbin(p[riverID[j]],N2[j])
    
    # Prediction
    N_pred[j]~dpois(lambda[j])
    DL1_pred[j]~dbin(p[riverID[j]],N_pred[j]) #C1 vecteur. y=1-> premier élemt C1. C1 varie tous les ans
    N2_pred[j]<-(N_pred[j]-DL1_pred[j])
    DL2_pred[j]~dbin(p[riverID[j]],N2_pred[j])
    C_pred[j] <- DL1_pred[j] + DL2_pred[j]
    
    # Capture probability
    #logit(p[riverID[j]]) <- delta + epsilonP[riverID[j]]
    # logit(p[riverID[j]]) <- logit_p[riverID[j]]
    # logit_p[riverID[j]]~dnorm(log_mup[riverID[j]], tauP)
    # #log_mup[riverID[j]] <- delta + epsilonP[riverID[j]]# /!\ we consider year effect instead of age because protocol and sampling effort could have change over time.
    # log_mup[riverID[j]] <- delta + epsilonP[riverID[j]]
  }#end boucle
  
  
  # PETERSEN
  for (j in n2[1]:n2[2]){
    #for (j in 1:n){ #y boucle sur les lignes du dataframe
    # Abundance
    N[j]~dpois(lambda[j]) #N tiré dans poisson dépend de param lambda
    #N[j] ~ dnegbin(q[j], r)
    #q[j] <- r / (r + lambda[j])  # Convert lambda to p[riverID[j]]
    
    # Density
    lambda[j]<-(dens[j]*(area[j]/100)) # number of fish / 100m2
    dens[j]~dlnorm(log(muD[j]), tauD)
    #muD[j] <- alpha_muD[riverID[j], year[j]]
    
    ##logistic
    #muD[j] <- kappa[riverID[j]] / (1+alpha[riverID[j]] * exp(beta[riverID[j]]*(year_capture[j])))
    #muD[j] <- kappa[riverID[j]] / (1+mu_alpha * exp(-beta[riverID[j]]*(year[j]))) #log
    muD[j] <- kappa / (1+alpha * exp(-beta*(popAge[j]))) #log
    #muD[j] <- kappa[riverID[j]] / (1+alpha * exp(-beta*(popAge[j]))) #log
    
    # Petersen method
    p[riverID[j]]~dbin(p[riverID[j]],N[j]) #C1 vecteur. y=1-> premier élemt C1. C1 varie tous les ans
    P2[j]~dbin(p[riverID[j]],N[j])
    
    # Prediction
    N_pred[j]~dpois(lambda[j])
    p_pred[j]~dbin(p[riverID[j]],N_pred[j]) #C1 vecteur. y=1-> premier élemt C1. C1 varie tous les ans
    P2_pred[j]~dbin(p[riverID[j]],N_pred[j])
    C_pred[j] <- p_pred[j] + P2_pred[j]
    
    # Capture probability
    #logit(p[riverID[j]]) <- delta + epsilonP[riverID[j]]
    # logit(p[riverID[j]]) <- logit_p[riverID[j]]
    # logit_p[riverID[j]]~dnorm(log_mup[riverID[j]], tauP)
    # #log_mup[riverID[j]] <- delta + epsilonP[riverID[j]]# /!\ we consider year effect instead of age because protocol and sampling effort could have change over time.
    # log_mup[riverID[j]] <- delta + epsilonP[riverID[j]]
  }#end boucle
  

  # # PE
  for (j in n3[1]:n3[2]){
    #for (j in 1:n){ #y boucle sur les lignes du dataframe
    # Abundance
    N[j]~dpois(lambda[j]) #N tiré dans poisson dépend de param lambda
    #N[j] ~ dnegbin(q[j], r)
    #q[j] <- r / (r + lambda[j])  # Convert lambda to p[riverID[j]]
    
    # Density
    lambda[j]<-(dens[j]*(area[j]/100)) # number of fish / 100m2
    dens[j]~dlnorm(log(muD[j]), tauD)
    #muD[j] <- alpha_muD[riverID[j], year[j]]
    
    ##logistic
    #muD[j] <- kappa[riverID[j]] / (1+alpha[riverID[j]] * exp(beta[riverID[j]]*(year_capture[j])))
    #muD[j] <- kappa[riverID[j]] / (1+mu_alpha * exp(-beta[riverID[j]]*(year[j]))) #log
    muD[j] <- kappa / (1+alpha * exp(-beta*(popAge[j]))) #log
    #muD[j] <- kappa[riverID[j]] / (1+alpha * exp(-beta*(popAge[j]))) #log
    
    # Petersen method
    PE[j]~dbin(p[riverID[j]],N[j]) #C1 vecteur. y=1-> premier élemt C1. C1 varie tous les ans
    
    # Prediction
    N_pred[j]~dpois(lambda[j])
    C_pred[j]~dbin(p[riverID[j]],N_pred[j]) #C1 vecteur. y=1-> premier élemt C1. C1 varie tous les ans
    
    # Capture probability
    #logit(p[riverID[j]]) <- delta + epsilonP[riverID[j]]
    # logit(p[riverID[j]]) <- logit_p[riverID[j]]
    # logit_p[riverID[j]]~dnorm(log_mup[riverID[j]], tauP)
    # #log_mup[riverID[j]] <- delta + epsilonP[riverID[j]]# /!\ we consider year effect instead of age because protocol and sampling effort could have change over time.
    # log_mup[riverID[j]] <- delta + epsilonP[riverID[j]]
  }#end boucle
  
  for (j in 1:n){
    area[j] ~ dlnorm(muS[riverID[j]],tauS);T(,2500)
  }
  
  
  ##PRIOR
  #delta~dnorm(0, 1)
  #pmoy<-ilogit(delta)
  #tau_epsilon <- pow(sigma_eps,-2)
  #sigma_eps ~dgamma(2/5,5/2)#~ dunif(0,2)
  for (i in 1:max(riverID)){
    muS[i]~dnorm(nu, tau_nu)#~dgamma(1, 1)
    #epsilonP[i]~dnorm(0,tau_epsilon) 
    
    #naive estimation
    # for (y in 1:maxMetapopAge){
    #   alpha_muD[i, y] ~ dlnorm(0, 1)
    # }
  }
  nu~dnorm(0, 0.01)
  tau_nu <- pow(sigma_nu,-2)
  sigma_nu ~dgamma(2/5,5/2)#~ dunif(0,1)
  
  #r ~ dgamma(1, 1)  # Prior for overdispersion
  
  kappa~dlnorm(3,4)
  #kappa ~ dlnorm(3.1, 1/(0.69*0.69)) # to keep 95% of kappa between [10; 50]
  #mu_kappa ~ dlnorm(3.1, 1/(0.41*0.41)) # to keep 95% of kappa between [10; 50]
  #tau_kappa <- pow(sigma_kappa,-2)
  #sigma_kappa ~ dunif(0,2)
  beta~dlnorm(0,1)
  alpha~dlnorm(0,1)
  
  t50 <- log(alpha)/beta # time to reach 50% of carrying capacity
  
  tauS <- pow(sigmaS,-2)# variance intra-annuelle
  sigmaS ~dgamma(2/5,5/2)#~ dunif(0,1)
  tauD <- pow(sigmaD,-2)# variance intra-annuelle
  sigmaD ~dgamma(2/5,5/2)#~ dunif(0,2)
  #tauP <- pow(sigmaP,-2)
  #sigmaP ~dgamma(2/5,5/2)#~ dunif(0,2)
  
  
  #for (pop in 1:max(riverID)){   
  #  P_pred[pop]<- ilogit(delta+ epsilonP[pop])
  #} # end loop pop
  
  for (t in 1:50){
    muD_pred[t] <- kappa / (1+alpha * exp(-beta*(t)))
  }
} #end model

