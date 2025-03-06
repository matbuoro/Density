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
    ##naif avec effet rivière et année
    muD[j] <- alpha_muD[riverID[j], year[j]]
    logit(p[j]) <- logit_p[j]
    logit_p[j]~dnorm(log_muP[j], tauP)
    # log_muP[j] <- delta + epsilonP[riverID[j]]# /!\ we consider year effect instead of age because protocol and sampling effort could have change over time.
    log_muP[j] <- delta[censusType[j]] + epsilonP[riverID[j]]
    area[j] ~ dlnorm(muS[riverID[j]],tauS);T(,2500)
  }#end boucle
  
  
  ##PRIOR
  
  for (i in 1:max(riverID)){
    muS[i]~dnorm(0, 0.01)#~dgamma(1, 1)
    epsilonP[i]~dnorm(0,tau_epsilon) 

    #naive estimation
    for (y in 1:maxMetapopAge){
      alpha_muD[i, y] ~ dlnorm(0, 1)
    }
  }

  tau_epsilon <- pow(sigma_eps,-2)
  sigma_eps ~ dunif(0,10)

  # logit(delta)<-logit_delta
  # logit_delta~dnorm(0, 1.5)
  # pmoy<-ilogit(delta)
  logit(delta[1])<-logit_delta1
  logit_delta1~dnorm(0, 1.5)
  pmoy_DL<-ilogit(delta[1])
  logit(delta[2])<-logit_delta2
  logit_delta2~dnorm(0, 1.5)
  pmoy_P<-ilogit(delta[2])
  logit(delta[3])<-logit_delta3
  logit_delta3~dnorm(0, 1.5)
  pmoy_PE<-ilogit(delta[3])
  #delta[2]~dnorm(0, 0.1)#~dgamma(1, 1)
  tauS <- pow(sigmaS,-2)# variance intra-annuelle
  sigmaS ~ dunif(0,1000)
  tauD <- pow(sigmaD,-2)# variance intra-annuelle
  sigmaD ~ dunif(0,1000)
  tauP <- pow(sigmaP,-2)
  sigmaP ~ dunif(0,10)


  
  
  for (pop in 1:max(riverID)){   
    P_pred[pop]<- ilogit(delta[censusType[pop]]+ epsilonP[pop])#+ (pow(t,delta[3])))
    # P_DL_pred[pop]<- ilogit(delta[1]+ epsilonP[pop])
    # P_P_pred[pop]<- ilogit(delta[2]+ epsilonP[pop])
    # P_PE_pred[pop]<- ilogit(delta[3]+ epsilonP[pop])
  } # end loop pop
} #end model

