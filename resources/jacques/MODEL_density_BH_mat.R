modelstat<-function(){

  for (j in 1:n){ #y boucle sur les lignes du dataframe
    # LIKELIHOOD
    N[j]~dpois(lambda[j]) #N tiré dans poisson dépend de param lambda
    DL1[j]~dbin(p[j],N[j]) #C1 vecteur. y=1-> premier élemt C1. C1 varie tous les ans

    N2[j]<-(N[j]-DL1[j])
    DL2[j]~dbin(p[j],N2[j])

    #N3[j]<-(N2[j]-DL2[j])
    #DL3[j]~dbin(p[j],N3[j])

    P1[j]~dbin(p[j],N[j])
    P2[j]~dbin(p[j],N[j])

    PE[j]~dbin(p[j],N[j]) #p différente ?

    lambda[j]<-(dens[j]*(area[j]/100)) # number of fish / 100m2

    dens[j]~dlnorm(log_muD[j], tauD)
    log_muD[j] <- log(muD[j])
    # Berverton-Holt
    muD[j] <- (kappa[riverID[j]]* pow(year_capture[j]+1, d[riverID[j]])) / (pow(beta[riverID[j]], d[riverID[j]]) + pow(year_capture[j]+1, d[riverID[j]]))

    ## Proba capture
    logit(p[j]) <- logit_p[j]
    logit_p[j]~dnorm(log_muP[j], tauP)
    log_muP[j] <- delta+  epsilonP[riverID[j]] # /!\ we consider year effect instead of age because protocol and sampling effort could have change over time.

    # likelihood for  the area. NB: not clear trend as a function of time or population age so don't bother. Like wise for river effect. 
    area[j] ~ dlnorm(muS[riverID[j]],tauS);T(,2500)
  }#end boucle


  # PRIOR

  for (i in 1:max(riverID)){

    muS[i]~dnorm(0, 0.1)#~dgamma(1, 1)
    kappa[i]~dnorm(mu_kappa, pow(sigma_kappa,-2));T(0,)
    beta[i] ~dexp(1)
    d[i]<-D
    epsilonP[i]~dnorm(0,tau_epsilon) #random effect for density

  }

  # root node priors - population means
  #BH
  mu_kappa ~ dunif(5,30)
  sigma_kappa ~ dunif(0,10)
  D~dbeta(2,2)

  delta<- ilogit(pmoy)
  pmoy~dbeta(4,2)

  tauS <- pow(sigmaS,-2)# variance intra-annuelle
  sigmaS ~ dunif(0,1000)
  tauD <- pow(sigmaD,-2)# variance intra-annuelle
  sigmaD ~ dunif(0,1000)
  tauP <- pow(sigmaP,-2)# variance intra-annuelle
  sigmaP ~ dunif(0,10)
   tau_epsilon <- pow(sigma_eps,-2)
   sigma_eps ~ dunif(0,10)
  # PREDICTION
  for (pop in 1:max(riverID)){

    P_pred[pop]<- ilogit(delta+ epsilonP[pop])#+ (pow(t,delta[3])))

    for (t in 1:(trueMaxPopAge[pop]+1)){

      #BH
      Dens_pred[pop,t] <- ((kappa[pop]* pow(t, d[pop])) / (pow(beta[pop], d[pop]) + pow(t, d[pop])))

    } # end loop t
  } # end loop riverID

} #end model