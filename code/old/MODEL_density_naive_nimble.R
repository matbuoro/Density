modelstat<-nimbleCode({
  
  # DE LURY
  for (j in n1[1]:n1[2]){ #y boucle sur les lignes du dataframe
    # Abundance
    #N[j]~dpois(lambda[j]) #N tiré dans poisson dépend de param lambda
    N[j] ~ dnegbin(q[j], r)
    q[j] <- r / (r + lambda[j])  # Convert lambda to p[j]
    
    # Density
    lambda[j]<-(dens[j]*(area[j]/100)) # number of fish / 100m2
    dens[j]~dlnorm(log(muD[j]), tauD)
    muD[j] <- alpha_muD[riverID[j], year[j]]
    
    # De Lury method
    DL1[j]~dbin(p1[j],N[j]) #C1 vecteur. y=1-> premier élemt C1. C1 varie tous les ans
    N2[j]<-(N[j]-DL1[j])
    DL2[j]~dbin(p1[j],N2[j])

    # Capture probability
    #logit(p1[j]) <- delta + epsilonP[riverID[j]]
    logit(p1[j]) <- logit_p1[j]
    logit_p1[j]~dnorm(log_muP1[j], tauP)
    #log_muP1[j] <- delta + epsilonP[riverID[j]]# /!\ we consider year effect instead of age because protocol and sampling effort could have change over time.
    log_muP1[j] <- delta[1] + epsilonP[riverID[j]]
  }#end boucle
  
  
  # # PETERSEN
  # for (j in n2[1]:n2[2]){ #y boucle sur les lignes du dataframe
  #   # Abundance
  #   #N[j]~dpois(lambda[j]) #N tiré dans poisson dépend de param lambda
  #   N[j] ~ dnegbin(q[j], r)
  #   q[j] <- r / (r + lambda[j])  # Convert lambda to p[j]
  #   
  #   # Density
  #   lambda[j]<-(dens[j]*(area[j]/100)) # number of fish / 100m2
  #   dens[j]~dlnorm(log(muD[j]), tauD)
  #   muD[j] <- alpha_muD[riverID[j], year[j]]
  #   
  #   # Petersen method
  #   P1[j]~dbin(p2[j],N[j]) 
  #   P2[j]~dbin(p2[j],N[j])
  #   
  #   # Capture probability
  #   #logit(p2[j]) <- delta + epsilonP[riverID[j]]
  #   logit(p2[j]) <- logit_p2[j]
  #   logit_p2[j]~dnorm(log_muP2[j], tauP)
  #   log_muP2[j] <- delta + epsilonP[riverID[j]]# /!\ we consider year effect instead of age because protocol and sampling effort could have change over time.
  #   #log_muP2[j] <- delta[2] #+ epsilonP[riverID[j]]
  # }#end boucle
  # 
  # 
  # # PE
  # for (j in n3[1]:n3[2]){ #y boucle sur les lignes du dataframe
  #   # Abundance
  #   N[j]~dpois(lambda[j]) #N tiré dans poisson dépend de param lambda
  #   #N[j] ~ dnegbin(q[j], r)
  #   #q[j] <- r / (r + lambda[j])  # Convert lambda to p[j]
  #   
  #   # Density
  #   lambda[j]<-(dens[j]*(area[j]/100)) # number of fish / 100m2
  #   dens[j]~dlnorm(log(muD[j]), tauD)
  #   muD[j] <- alpha_muD[riverID[j], year[j]]
  #   
  #   # PE only
  #   PE[j]~dbin(p3[j],N[j]) #C1 vecteur. y=1-> premier élemt C1. C1 varie tous les ans
  #   
  #   # Capture probability
  #   #logit(p3[j]) <- delta + epsilonP[riverID[j]]
  #   logit(p3[j]) <- logit_p3[j]
  #   logit_p3[j]~dnorm(log_muP3[j], tauP)
  #   log_muP3[j] <- delta + epsilonP[riverID[j]]# /!\ we consider year effect instead of age because protocol and sampling effort could have change over time.
  #   #log_muP3[j] <- delta[3] #+ epsilonP[riverID[j]]
  # }#end boucle
  
  for (j in 1:n){
  area[j] ~ T(dlnorm(muS[riverID[j]],tauS),,2500)
  }
  
  
  ##PRIOR
  for (i in 1:max(riverID)){
    muS[i]~dnorm(0, 0.01)#~dgamma(1, 1)
    epsilonP[i]~dnorm(0,tau_epsilon) 

    #naive estimation
    for (y in 1:maxMetapopAge){
      alpha_muD[i, y] ~ dlnorm(0, 1)
    }
  }

  r ~ dgamma(1, 1)  # Prior for overdispersion
  
  tau_epsilon <- pow(sigma_eps,-2)
  sigma_eps ~ dunif(0,10)

  # logit(delta)<-logit_delta
  # logit_delta~dnorm(0, 1.5)
  # pmoy<-ilogit(delta)
  logit(delta[1])<-logit_delta1
  logit_delta1~dnorm(0, 1.5)
  pmoy_DL<-ilogit(delta[1])
  # logit(delta[2])<-logit_delta2
  # logit_delta2~dnorm(0, 1.5)
  # pmoy_P<-ilogit(delta[2])
  # logit(delta[3])<-logit_delta3
  # logit_delta3~dnorm(0, 1.5)
  # pmoy_PE<-ilogit(delta[3])
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
}) #end model

