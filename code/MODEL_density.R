modelstat<-function(){
  #boucle
  for (j in 1:n){ #y boucle sur les lignes du dataframe
    # LIKELIHOOD
    N[j]~dpois(lambda[j]) #N tiré dans poisson dépend de param lambda

    # 1. De Lury method
    DL1[j]~dbin(p[j],N[j]) #C1 vecteur. y=1-> premier élemt C1. C1 varie tous les ans
    
    N2[j]<-(N[j]-DL1[j])
    DL2[j]~dbin(p[j],N2[j])
    
    #N3[j]<-(N2[j]-DL2[j])
    #DL3[j]~dbin(p[j],N3[j])
    
    # 2. Petersen method
    P1[j]~dbin(p[j],N[j])
    P2[j]~dbin(p[j],N[j])

    # 3. Electrofishing
    #PE[j]~dbin(p[j],N[j])

    #ancien prior = hyperparamètres
    #p[year[j]]~dbeta(2,2)
    lambda[j]<-(dens[j]*(area[j]/100)) # number of fish / 100m2
    #dens[j]~dgamma(1,1)
    #dens[j]~dnorm(muD[year[j]], tauD[year[j]]);T(0,)#gamma défini positif 
    #dens[j]~dlnorm(log_muD[j], tauD)
    #log_muD[j] <- gamma[1]+gamma[2]*(popAge[j] - mean(popAge[])) + epsilonD[riverID[j]] # (pow(year[j],gamma[2]))#+(pow(year[j],gamma[2+AGE[j]]))
    #log_muD[j] <- pow(year[j] - mean(year[]),gamma[2])# (pow(year[j],gamma[2]))#+(pow(year[j],gamma[2+AGE[j]]))
    dens[j]~dlnorm(log_muD[riverID[j], popAge[j]+1], tauD)

   ## Generalized Logistic density function
    #dens[j]~dnorm(muD[j], tauD)
    #muD[j] <- A + (Kappa-A) / pow(C+Q*exp(-B*popAge[j]), 1/v)
    #muD[j] <- Kappa * (1 / pow(1+Q*exp(-B*popAge[j]), 1/nu))
    #log_muD[j] <- log(muD[j])
    #muD[j] <- kappa[riverID[j]] / (1+exp(-beta[riverID[j]]*log(popAge[j])))
    #muD[j] <- kappa[riverID[j]] / (1+alpha[riverID[j]] * exp(beta[riverID[j]]*(year_capture[j])))#+(t0[j]-1))))
    #muD[j] <- kappa[riverID[j]] / (1+alpha[riverID[j]] * exp(-beta[riverID[j]]*(year[j]-1962)))
    #muD[j] <- theta[3] / (1+exp(theta[1]+theta[2]*(popAge[j]-t0)))

 #muD[j] <- (kappa[riverID[j]]* alpha[riverID[j]]) / (alpha[riverID[j]] + (kappa[riverID[j]] - alpha[riverID[j]])*pow(beta[riverID[j]], -popAge[j]))

# Berverton-Holt
#muD[j] <- (kappa[riverID[j]]* pow(year_capture[j]+1, d[riverID[j]])) / (pow(beta[riverID[j]], d[riverID[j]]) + pow(year_capture[j]+1, d[riverID[j]]))


    #t0[j]~dcat(pCol[1:10])

    ## Proba capture
    logit(p[j]) <- logit_p[j]
    logit_p[j]~dnorm(log_muP[j], tauP)
    log_muP[j] <- delta[1]+ delta[2]*year[j] + epsilonP[riverID[j]] # /!\ we consider year effect instead of age because protocol and sampling effort could have change over time.
    
    # likelihood for  the area. NB: not clear trend as a function of time or population age so don't bother. Like wise for river effect. 
    area[j] ~ dlnorm(muS[riverID[j]],tauS);T(,2500)
  }#end boucle


  
  #pCol[1:10] ~ ddirch(a[1:10])
  #for (i2 in 1:10){
  #  a[i2]<-1/10
  #}
  #t0 <- 0
  #Ymax <- kappa
  #Ymin <- kappa / (1 + alpha)
  #Ghalf <- -1 * (kappa * beta) / 4
  
  #kappa <- exp(theta[1])
  #alpha <- exp(theta[2]) - 1
  #beta <- -1  * exp(theta[3])

    # root node prior - parameters
  #theta[1] ~ dnorm(0, 0.001)
  #theta[2] ~ dnorm(0, 0.001)
  #theta[3] ~ dnorm(0, 0.001);T(0,)

   q <- 0.25
# PRIOR
  for (i in 1:max(riverID)){

    #muD[i]~dnorm(0, 0.1)#~dgamma(1, 1)
    muS[i]~dnorm(0, 0.1)#~dgamma(1, 1)

    #epsilonD[i]~dnorm(0,tau_epsilon[1]) #random effect for density
    epsilonP[i]~dnorm(0,tau_epsilon[2]) #random effect for capture probability

    #kappa[i] ~dgamma(a[1],b[1])#<- mu_kappa #~dlnorm(0, 0.1)
    kappa[i]~dgamma(1,1)
    alpha[i] ~dexp(1)
    beta[i] ~dgamma(1, 1)#~dgamma(1,1/s[3])

    #d[i]~dexp(1)
 
 
    #d[i] <- (log(q) + log(beta[i]) -log(tq[i]))/(log(1-q))
    #tq[i] ~dunif(1,20)
    #kappa[i] <- theta[3]
    #alpha[i] <- theta[1]
    #beta[i] <- 1#theta[2]
    #alpha <- (kappa/P0 -1)*exp(beta*t0)
    #beta[i] <- mu_beta
    #beta[i] ~ dnorm(0, 0.1)
    #beta[i] ~ dnorm(mu_beta, tau_beta)
    #beta[i]~dgamma(2,1/s[2])
  
    #alpha[i]~dnorm(mu_alpha,tau_alpha)
    #kappa[i]~dnorm(mu_kappa,tau_kappa)

    # reparameterization to real line
    #kappa[i] <- exp(theta.k[i])
    #alpha[i] <- exp(theta.a[i]) - 1
    #beta[i] <- -1 * exp(theta.b[i])
    
    # reparameterization to meaningful parameters
    #ymax[i] <- kappa[i]
    #ymin[i] <- kappa[i] / (1 + alpha[i])
    #ghalf[i] <- -1 * (kappa[i] * beta[i]) / 4
    
    #theta.a[i] ~ dnorm(mu_alpha, tau_alpha)
    #theta.b[i] ~ dnorm(mu_beta, tau_beta)
    #theta.k[i] <- mu_kappa #~ dnorm(mu_kappa, tau_kappa)
  }

#s[1]<-10#~dchisqr(1)
#s[2]~dchisqr(1)
#s[3]~dchisqr(1)
#s[4]<- 0.5#~dchisqr(1)

# root node priors - population means
  mu_alpha ~ dunif(0, 50)
  #  mu_alpha~dgamma(2,1/s[2])
  #  s[2]~dchisqr(2)
  mu_beta ~ dunif(0, 20)
  mu_kappa ~ dunif(10,30)
  #mu_kappa~dgamma(2,1/s[1])
  #s[1]~dchisqr(2)
  #mu_beta~dgamma(2,1/s[2])
  #s[2]~dchisqr(2)

  tau_epsilon[1] <- pow(sigma_eps[1],-2)
  sigma_eps[1] ~ dunif(0,10)
  tau_epsilon[2] <- pow(sigma_eps[2],-2)
  sigma_eps[2] ~ dunif(0,10)
  #tau_alpha <- pow(sigma_alpha,-2)
  #sigma_alpha ~ dunif(0,100)
  #tau_kappa <- pow(sigma_kappa,-2)#
  #sigma_kappa ~ dunif(0,100)
  #tau_beta <- pow(sigma_beta,-2)
  #sigma_beta ~ dunif(0,100)

  
  #gamma[1]~dnorm(0, 0.1)
  #gamma[2]~dnorm(0, 0.1)#;T(0,)#~dgamma(1, 1)
  #gamma[3]~dnorm(0, 0.1)#;T(0,)#~dgamma(1, 1)
  #delta[1]~dnorm(0, 0.1)
  delta[1]<- ilogit(pmoy)
  pmoy~dbeta(4,2)
  delta[2]~dnorm(0, 0.1)#~dgamma(1, 1)
  #delta[3]~dnorm(0, 0.1)
  #muS~dnorm(0, 0.1)#~dgamma(1, 1)
  tauS <- pow(sigmaS,-2)# variance intra-annuelle
  sigmaS ~ dunif(0,1000)
  tauD <- pow(sigmaD,-2)# variance intra-annuelle
  sigmaD ~ dunif(0,1000)
  tauP <- pow(sigmaP,-2)# variance intra-annuelle
  sigmaP ~ dunif(0,10)

# A: the left horizontal asymptote;
# K: the right horizontal asymptote when C=1. If A=0 and  C=1 then K is called the carrying capacity;
# B}: the growth rate;
# nu >0 : affects near which asymptote maximum growth occurs.
#Q: is related to the value Y(0)
#A<-0 # the left horizontal asymptote;
#C<-1
#kappa~dgamma(2,1/s)
#s~dchisqr(2)
#alpha~dnorm(0,0.1)
#mu_kappa~dnorm(0,0.1)
#mu_alpha~dnorm(0,0.1)
#beta <-1#~dgamma(0.1, 0.1)
#nu~dgamma(0.1,0.1)


 # PREDICTION
  for (pop in 1:max(riverID)){
    #for (t in 1:max(popAge)){
      for (t in 1:(maxPopAge[pop]+1)){

  log_muD[pop, t]~dnorm(0, 0.001)
  muD[pop, t] <- exp(log_muD[pop, t])

    #Dens_pred[pop,t]<- exp(gamma[1]+gamma[2]*(t - mean(popAge[])) + epsilonD[pop])
    #Dens_pred[pop,t] <-  kappa[pop] / (1+exp(-beta[pop]*log(t)))
    #Dens_pred[pop,t] <-  kappa[pop] / (1+alpha[pop]*exp(beta[pop]*(t-1)))

    #Dens_pred[pop,t] <- ((kappa[pop]* pow(t, d[pop])) / (pow(beta[pop], d[pop]) + pow(t, d[pop])))

    P_pred[pop,t]<- ilogit(delta[1]+ delta[2]*t + epsilonP[pop])#+ (pow(t,delta[3])))
    #muP[t,2]<- ilogit(delta[1]+ delta[2]) #+ (pow(t,delta[3])))

  } # end loop t
  } # end loop pop

# Over all populations
for (t in 1:max(popAge)){
  #Dens_pred_all[t]<- exp(gamma[1]+gamma[2]*(t - mean(popAge[])))
    #Dens_pred_all[t] <-  mu_kappa / (1+exp(-mu_beta*log(t)))
    #Dens_pred_all[t] <- theta[3] / (1+ exp(theta[1]+theta[2]*(t-t0)))
    P_pred_all[t]<- ilogit(delta[1]+ delta[2]*t)
} # end loop t

} #end model
