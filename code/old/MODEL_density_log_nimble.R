modelstat<-nimbleCode({

  
  ## DE LURY
  for (j in n1[1]:n1[2]){ #y boucle sur les lignes du dataframe
    
    N[j]~dpois(lambda[j]) #N tiré dans poisson dépend de param lambda
    #N[j] ~ dnegbin(q[j], r)
    #q[j] <- r / (r + lambda[j])  # Convert lambda to p[j]
    
    DL1[j]~dbin(p[j],N[j]) #C1 vecteur. y=1-> premier élemt C1. C1 varie tous les ans
    N2[j]<-(N[j]-DL1[j])
    DL2[j]~dbin(p[j],N2[j])
    
    #P1[j]~dbin(p[j],N[j])
    #P2[j]~dbin(p[j],N[j])
    #PE[j]~dbin(p[j],N[j]) 
    
    lambda[j]<-(dens[j]*(area[j]/100)) # number of fish / 100m2
    dens[j]~dlnorm(log_muD[j], tauD)
    log_muD[j] <- log(muD[j])
    ##logistic
    #muD[j] <- kappa[riverID[j]] / (1+alpha[riverID[j]] * exp(beta[riverID[j]]*(year_capture[j])))
    #muD[j] <- kappa[riverID[j]] / (1+mu_alpha * exp(-beta[riverID[j]]*(year[j]))) #log
    muD[j] <- kappa / (1+alpha * exp(-beta*(popAge[j]))) #log
    #muD[j] <- kappa[riverID[j]] / (1+alpha * exp(-beta*(popAge[j]))) #log
    
    ## Proba capture
    logit(p[j]) <- logit_p[j]
    logit_p[j]~dnorm(log_muP[j], tauP)
    log_muP[j] <- delta + epsilonP[riverID[j]] # /!\ we consider year effect instead of age because protocol and sampling effort could have change over time.
    
  }#end boucle
  
  # Estimating area
  for (k in 1:n){
    #area[k] ~ dlnorm(muS,tauS);T(,2500)
    area[k] ~ T(dlnorm(muS[riverID[k]],tauS),0,2500)
  }
  
  ##PRIOR
  for (i in 1:max(riverID)){
    muS[i]~dnorm(0, 0.01)#~dgamma(1, 1)
    
    #kappa[i]~dlnorm(mu_kappa,tau_kappa)
    #alpha[i]~dnorm(mu_alpha,tau_alpha)
    #beta[i] ~ dnorm(mu_beta, tau_beta)
    
    #t50[i] <- log(alpha)/beta # time to reach 50% of carrying capacity
    
    epsilonP[i]~dnorm(0,tau_epsilon) #random effect for capture probability
  }
  t50 <- log(alpha)/beta # time to reach 50% of carrying capacity
  
  r ~ dgamma(1, 1)  # Prior for overdispersion
  
  kappa ~ dlnorm(3.1, 1/(0.69*0.69)) # to keep 95% of kappa between [10; 50]
  #tau_kappa <- pow(sigma_kappa,-2)
  #sigma_kappa ~ dunif(0,2)
  #mu_kappa ~ dlnorm(3.1, 1/(0.41*0.41)) # to keep 95% of kappa between [10; 50]

  beta~T(dnorm(0,0.1),0,)
  alpha~dlnorm(0,1)
  #alpha~dunif(1,40) 

  delta~dnorm(0, 1.5)
  pmoy<-ilogit(delta)

  tau_epsilon <- pow(sigma_eps,-2)
  sigma_eps ~ dunif(0,2)
  tauS <- pow(sigmaS,-2)# variance intra-annuelle
  sigmaS ~ dunif(0,2)
  tauD <- pow(sigmaD,-2)# variance intra-annuelle
  sigmaD ~ dunif(0,1)
  tauP <- pow(sigmaP,-2)
  sigmaP ~ dunif(0,2)

  
  for (pop in 1:max(riverID)){   
    P_pred[pop]<- ilogit(delta+ epsilonP[pop])#+ (pow(t,delta[3])))
    # for (t in 1:(trueMaxPopAge[pop]+1)){
    # # Dens_pred[pop,t] <-  kappa[pop] / (1+alpha[pop]*exp(beta[pop]*(t-1)))
    #   Dens_pred[pop,t] <-  kappa[pop] / (1+mu_alpha*exp(-beta[pop]*(t-1)))
    # #   
    #  } # end loop t
  } # end loop pop
  
  for (t in 1:50){
  muD_pred[t] <- kappa / (1+alpha * exp(-beta*(t)))
}
  }) #end model

