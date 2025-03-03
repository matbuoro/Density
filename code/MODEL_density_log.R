modelstat<-function(){
  for (j in 1:n){ #y boucle sur les lignes du dataframe
    N[j]~dpois(lambda[j]) #N tiré dans poisson dépend de param lambda
    DL1[j]~dbin(p[j],N[j]) #C1 vecteur. y=1-> premier élemt C1. C1 varie tous les ans
    N2[j]<-(N[j]-DL1[j])
    DL2[j]~dbin(p[j],N2[j])
    P1[j]~dbin(p[j],N[j])
    P2[j]~dbin(p[j],N[j])
    PE[j]~dbin(p[j],N[j]) 
    lambda[j]<-(dens[j]*(area[j]/100)) # number of fish / 100m2
    dens[j]~dlnorm(log_muD[j], tauD)
    log_muD[j] <- log(muD[j])
    ##naif avec effet rivière
    #muD[j] <- alpha_muD[riverID[j]]
    ##log
    #muD[j] <- kappa[riverID[j]] / (1+alpha[riverID[j]] * exp(beta[riverID[j]]*(year_capture[j])))
    #muD[j] <- kappa[riverID[j]] / (1+mu_alpha * exp(-beta[riverID[j]]*(year[j]))) #log
    muD[j] <- kappa[riverID[j]] / (1+mu_alpha * exp(-mu_beta*(year[j]))) #log
    logit(p[j]) <- logit_p[j]
    logit_p[j]~dnorm(log_muP[j], tauP)
    log_muP[j] <- delta + epsilonP[riverID[j]] # /!\ we consider year effect instead of age because protocol and sampling effort could have change over time.
    area[j] ~ dlnorm(muS[riverID[j]],tauS);T(,2500)
  }#end boucle
  
  
  ##PRIOR

  # #estimation naive densite
  # for (j in 1:n){
  # 
  #   muD[j]~dnorm(0,0.01);T(0,)
  # 
  #   }

  
  for (i in 1:max(riverID)){
    muS[i]~dnorm(0, 0.01)#~dgamma(1, 1)
    #beta[i] ~ dnorm(mu_beta, tau_beta)
    kappa[i]~dlnorm(mu_kappa,tau_kappa)
    epsilonP[i]~dnorm(0,tau_epsilon) #random effect for capture probability
    # alpha[i]~dnorm(mu_alpha,tau_alpha)
  }
  
  beta_pred~ dnorm(mu_beta, tau_beta)
  # tau_epsilon[1] <- pow(sigma_eps[1],-2)
  # sigma_eps[1] ~ dunif(0,10)
  tau_epsilon <- pow(sigma_eps,-2)
  #tau_epsilon[2] <- pow(sigma_eps[2],-2)
  sigma_eps ~ dunif(0,10)
  #sigma_eps[2] ~ dunif(0,10)
  tau_beta ~ dgamma(5,0.1)
  # tau_beta <- pow(sigma_beta,-2)
  # sigma_beta ~ dunif(0,2)
  # tau_alpha <- pow(sigma_alpha,-2)
  sigma_alpha ~ dunif(0,100)
  tau_kappa <- pow(sigma_kappa,-2)
  sigma_kappa ~ dunif(0,5)
  mu_kappa ~ dnorm(0, 1)#~dunif(1,100)
  #mu_kappa <- pre_kappa*30
  pre_kappa~dbeta(2,2)
  mu_beta~dnorm(0.1,1)
  mu_alpha~dunif(1,40) 
  # mu_kappa~dgamma(2,1/s)
  # s~dchisqr(2)
  # mu_beta~dgamma(2,1/s)
  # mu_alpha~dgamma(2,1/s)
  #delta<- ilogit(pmoy)
  #pmoy~dbeta(4,2)
  logit(delta)<-logit_delta
  logit_delta~dnorm(0, 1.5)
  pmoy<-ilogit(delta)
  #delta[2]~dnorm(0, 0.1)#~dgamma(1, 1)
  tauS <- pow(sigmaS,-2)# variance intra-annuelle
  sigmaS ~ dunif(0,1000)
  tauD <- pow(sigmaD,-2)# variance intra-annuelle
  sigmaD ~ dunif(0,1000)
  tauP <- pow(sigmaP,-2)
  sigmaP ~ dunif(0,10)
  # tauP[2] <- pow(sigmaP[2],-2)
  # sigmaP[2] ~ dunif(0,10)
  #nu~dgamma(0.1,0.1)
  
  for (pop in 1:max(riverID)){   
    P_pred[pop]<- ilogit(delta+ epsilonP[pop])#+ (pow(t,delta[3])))
    # for (t in 1:(trueMaxPopAge[pop]+1)){
    # # Dens_pred[pop,t] <-  kappa[pop] / (1+alpha[pop]*exp(beta[pop]*(t-1)))
    #   Dens_pred[pop,t] <-  kappa[pop] / (1+mu_alpha*exp(-beta[pop]*(t-1)))
    # #   
    #  } # end loop t
  } # end loop pop
#   for (j in 1:n){ 
#   muD_pred[j] <- kappa[riverID[j]] / (1+mu_alpha * exp(-beta[riverID[j]]*(year[j])))
# }
  } #end model

