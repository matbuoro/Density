modelstat<-function(){
  for (j in 1:n){ #y boucle sur les lignes du dataframe
    N[j]~dpois(lambda[j]) #N tiré dans poisson dépend de param lambda
    DL1[j]~dbin(p[j],N[j]) #C1 vecteur. y=1-> premier élemt C1. C1 varie tous les ans
    N2[j]<-(N[j]-DL1[j])
    DL2[j]~dbin(p[j],N2[j])
    P1[j]~dbin(p[j],N[j])
    P2[j]~dbin(p[j],N[j])
    PE[j]~dbin(p[j],N[j]) #p différente ?
    lambda[j]<-(dens[j]*(area[j]/100)) # number of fish / 100m2
    dens[j]~dlnorm(log_muD[j], tauD)
    log_muD[j] <- log(muD[j])
   #muD[j] <- kappa[riverID[j]] / (1+alpha[riverID[j]] * exp(-beta[riverID[j]]*(year_capture[j])))
    muD[j] <- kappa[riverID[j]] / (1+mu_alpha * exp(-beta[riverID[j]]*(year_capture[j]))) 
    logit(p[j]) <- logit_p[j]
    logit_p[j]~dnorm(log_muP[j], tauP)
    log_muP[j] <- delta[1]+  epsilonP[riverID[j]] # /!\ we consider year effect instead of age because protocol and sampling effort could have change over time.
    area[j] ~ dlnorm(muS[riverID[j]],tauS);T(,2500)
  }#end boucle
  for (i in 1:max(riverID)){
    #muS[i]~dnorm(0, 0.1)#~dgamma(1, 1)
    muS[i]~dnorm(0, 0.01)# probably was not enough for area.
    epsilonP[i]~dnorm(0,tau_epsilon) #random effect for capture probability
   #epsilonD[i]~dnorm(0,tau_epsilonD) #random effect for density  
   beta[i] ~ dnorm(mu_beta, tau_beta)
   # alpha[i]~dnorm(mu_alpha,tau_alpha)
    kappa[i]~dnorm(mu_kappa,tau_kappa)
  }
    tau_epsilon <- pow(sigma_eps,-2)
  sigma_eps ~ dunif(0,10)
  #tau_epsilonD <- pow(sigma_epsD,-2)
  #sigma_epsD ~ dunif(0,1)
  tau_beta ~ dgamma(5,0.1) 
  #tau_beta <- pow(sigma_beta,-2) 
  sigma_beta ~ dunif(0,2) # not too variable or invalid parents error.
  tau_alpha <- pow(sigma_alpha,-2)
  sigma_alpha ~ dunif(0,2)  # not too variable or invalid parents error.
  tau_kappa <- pow(sigma_kappa,-2)
  sigma_kappa ~ dunif(0,2) # not too variable or invalid parents error.
  #tau_alpha <- pow(sigma_alpha,-2)
  #sigma_alpha ~ dunif(0,100)
  #tau_kappa <- pow(sigma_kappa,-2)
  #sigma_kappa ~ dunif(0,100)   
  mu_kappa~dunif(1,100) 
  mu_beta~dnorm(0.1,1)
  mu_alpha~dunif(1,40) 
  #mu_kappa~dgamma(4,0.2) 
  #mu_beta~dgamma(10,30)
  #mu_alpha~dgamma(4,1) 
  #tau_beta <- pow(sigma_beta,-2)
  #sigma_beta ~ dunif(0,100)
  #gamma[1]~dnorm(0, 0.1)
  #gamma[2]~dnorm(0, 0.1)#;T(0,)#~dgamma(1, 1)
  delta[1]<- ilogit(pmoy)
  pmoy~dbeta(4,2)
  delta[2]~dnorm(0, 0.1)#~dgamma(1, 1)
  tauS <- pow(sigmaS,-2)# variance intra-annuelle
  sigmaS ~ dunif(0,1000)
  tauD <- pow(sigmaD,-2)# variance intra-annuelle
  sigmaD ~ dunif(0,1000)
  tauP <- pow(sigmaP,-2)# variance intra-annuelle
  sigmaP ~ dunif(0,10)
  nu~dgamma(0.1,0.1)
  
  for (pop in 1:max(riverID)){   
    P_pred[pop]<- ilogit(delta[1]+ epsilonP[pop])#+ (pow(t,delta[3])))
    for (t in 1:(trueMaxPopAge[pop]+1)){
       Dens_pred[pop,t] <-  kappa[pop] / (1+mu_alpha*exp(-beta[pop]*(t-1)))
     
     # Dens_pred[pop,t] <-  kappa[pop] / (1+alpha[pop]*exp(beta[pop]*(t-1)))
    } # end loop t
  } # end loop pop
} #end model

