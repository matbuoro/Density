modelstat<-function()
{
  #boucle
  for (j in 1:n){ #y boucle sur les lignes du dataframe
    # LIKELIHOOD
    N[j]~dpois(lambda[j]) #N tiré dans poisson dépend de param lambda
    DL1[j]~dbin(p[j],N[j]) #C1 vecteur. y=1-> premier élemt C1. C1 varie tous les ans
    
    N2[j]<-(N[j]-DL1[j])
    DL2[j]~dbin(p[j],N2[j])
    
    #N3[j]<-(N2[j]-DL2[j])
    #DL3[j]~dbin(p[j],N3[j])
    
    
    #ancien prior = hyperparamètres
    #p[year[j]]~dbeta(2,2)
    lambda[j]<-d[j]*area[j]
    #d[j]~dgamma(1,1)
    #d[j]~dnorm(muD[year[j]], tauD[year[j]]);T(0,)#gamma défini positif 
    #d[j]~dlnorm(log_muD[j], tauD)
    #log_muD[j] <- gamma[1]+gamma[2]*(popAge[j] - mean(popAge[])) + epsilonD[riverID[j]] # (pow(year[j],gamma[2]))#+(pow(year[j],gamma[2+AGE[j]]))
    #log_muD[j] <- pow(year[j] - mean(year[]),gamma[2])# (pow(year[j],gamma[2]))#+(pow(year[j],gamma[2+AGE[j]]))

   ## Generalized Logistic density function
    d[j]~dnorm(muD[j], tauD)
    #muD[j] <- A + (Kappa-A) / pow(C+Q*exp(-B*popAge[j]), 1/v)
    #muD[j] <- Kappa * (1 / pow(1+Q*exp(-B*popAge[j]), 1/nu))
    muD[j] <- kappa / (1+alpha[riverID[j]] * exp(-log(popAge[j])))

    ## Proba capture
    logit(p[j]) <- logit_p[j]
    logit_p[j]~dnorm(log_muP[j], tauP)
    log_muP[j] <- delta[1]+ delta[2]*year[j] + epsilonP[riverID[j]] # /!\ we consider year effect instead of age because protocol and sampling effort could have change over time.
    
    # likelihood for  the area. NB: not clear trend as a function of time or population age so don't bother. Like wise for river effect. 
    area[j] ~ dlnorm(muS[riverID[j]],tauS)
  }#end boucle
  

# PRIOR
  for (i in 1:max(riverID)){
    muS[i]~dnorm(0, 0.1)#~dgamma(1, 1)

    alpha[i]~dnorm(mu_alpha,tau_alpha)

    epsilonD[i]~dnorm(0,tau_epsilon[1]) #random effect for density
    epsilonP[i]~dnorm(0,tau_epsilon[2]) #random effect for capture probability
  }
  tau_epsilon[1] <- pow(sigma_eps[1],-2)# variance intra-annuelle
  sigma_eps[1] ~ dunif(0,10)
  tau_epsilon[2] <- pow(sigma_eps[2],-2)# variance intra-annuelle
  sigma_eps[2] ~ dunif(0,10)
    tau_alpha <- pow(sigma_alpha,-2)# variance intra-annuelle
  sigma_alpha[1] ~ dunif(0,10)

  
  gamma[1]~dnorm(0, 0.1)
  gamma[2]~dnorm(0, 0.1)#;T(0,)#~dgamma(1, 1)
  #gamma[3]~dnorm(0, 0.1)#;T(0,)#~dgamma(1, 1)
  #delta[1]~dnorm(0, 0.1)
  delta[1]<- ilogit(pmoy)
  pmoy~dbeta(4,2)
  delta[2]~dnorm(0, 0.1)#~dgamma(1, 1)
  #delta[3]~dnorm(0, 0.1)
  #muS~dnorm(0, 0.1)#~dgamma(1, 1)
  tauS <- pow(sigmaS,-2)# variance intra-annuelle
  sigmaS ~ dunif(0,100)
  tauD <- pow(sigmaD,-2)# variance intra-annuelle
  sigmaD ~ dunif(0,10)
  tauP <- pow(sigmaP,-2)# variance intra-annuelle
  sigmaP ~ dunif(0,10)

# A: the left horizontal asymptote;
# K: the right horizontal asymptote when C=1. If A=0 and  C=1 then K is called the carrying capacity;
# B}: the growth rate;
# nu >0 : affects near which asymptote maximum growth occurs.
#Q: is related to the value Y(0)
#A<-0 # the left horizontal asymptote;
#C<-1
kappa~dgamma(2,1/s)
s~dchisqr(2)
#alpha~dnorm(0,0.1)
mu_alpha~dnorm(0,0.1)
beta <-1#~dgamma(0.1, 0.1)
#nu~dgamma(0.1,0.1)


 # PREDICTION
  for (pop in 1:max(riverID)){
    for (t in 1:max(popAge)){

    #Dens_pred[pop,t]<- exp(gamma[1]+gamma[2]*(t - mean(popAge[])) + epsilonD[pop])
    Dens_pred[pop,t] <-  kappa / (1+alpha[pop]*exp(-log(t)))

    P_pred[pop,t]<- ilogit(delta[1]+ delta[2]*t + epsilonP[pop])#+ (pow(t,delta[3])))
    #muP[t,2]<- ilogit(delta[1]+ delta[2]) #+ (pow(t,delta[3])))

  } # end loop t
  } # end loop pop

# Over all populations
for (t in 1:max(popAge)){
  #Dens_pred_all[t]<- exp(gamma[1]+gamma[2]*(t - mean(popAge[])))
    Dens_pred_all[t] <-  kappa / (1+mu_alpha*exp(-log(t)))
} # end loop t

}#end model