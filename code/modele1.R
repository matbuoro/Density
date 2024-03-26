rm(list=ls())   # Clear memory


#setwd( "C:/Users/Leo Ball/Desktop/Kerguelen")

## Load libraries
library(rjags)
library(MCMCvis)


## Load data
donneespropres <- read.delim("data/donneespropres.txt")

dates = as.Date(donneespropres$Date, format = "%d/%m/%Y")
donneespropres <-transform(donneespropres, Date  = dates )
donneespropres$year<-as.numeric(format(as.Date(dates), "%Y"))

donneespropres <-donneespropres[order(donneespropres$Date),]


espece = 'ST'
age = "1"
df <- subset(donneespropres,Espece == espece & Age == age)

sum(df$DL1)
sum(df$DL2)
levels(as.factor(df$Secteur))
levels(as.factor(df$year))



data <- list(
  N = nrow(df),
  C1 = df$DL1 *10,
  C2 = df$DL2 *10,
  area = df$Surface,
  hab = df$Hab
#  a = donneespropres$Age,
#  e = donneespropres$Espece,
# sec = donneespropres$Secteur
#  d = donneespropres$Date
  
  )

## Initial values for unknown parameters (prior)
inits = function(){list(
  mu_dens=rgamma(1,10,.01)
  #alpha = rnorm(1,0,1),
  #beta = rnorm(1,0,1),
  #gamma = rnorm(1,0,1)
)}


## Parameters to monitor
parameters <- c("p","dens"
                ,"mu_dens","rate_dens","shape_dens"
                ,"alpha","beta","gamma"
                #,"p_radier","p_pool"
                #,"a","b"
                )

# DEFINE the model
mymodel <- "
model{

## Density
mu_dens ~ dgamma(1,0.1) ## Mean density 
rate_dens ~ dgamma(0.1,0.1) #### Inverse of a Gamma distribution for density
shape_dens <- mu_dens * rate_dens # shape parameter of the gamma distribution

for (i in 1:N){

## Likelihood 

    ntot[i] ~ dpois(psi_n[i]) # total population size
    psi_n[i] <- dens[i] * area[i] # dens/m2
    dens[i] ~ dgamma(shape_dens,rate_dens)
    #dens[i] ~ dunif(0,100)
    
    ## Successive removals
    C1[i] ~ dbin(p[i],ntot[i]) # first pass
    n1[i] <- ntot[i]-C1[i] # fish not captured during the first pass

    C2[i] ~ dbin(p[i],n1[i]) # second pass

# capture probability
#p[i] ~ dbeta(a[hab[i]+1], b[hab[i]+1])
logit(p[i]) <- alpha + beta*hab[i] + gamma*(area[i] -mean(area[]))

# density estimates
#log(lambda[i]) <- alpha_lambda[i] #+ beta_lambda*X[i]
#alpha_lambda[i] ~ dnorm(0,0.001)
#lambda[i]~dunif(0,100) # ici les densit?s sont ind?pendantes
#lambda[i]~dgamma(0.01,0.01)


}

## Prior
#p ~ dbeta(1, 1)
alpha ~ dnorm(0,0.001)
beta ~ dnorm(0,0.001)
gamma ~ dnorm(0,0.001)

a~dgamma(0.1,0.1)
b~dgamma(0.1,0.1)


#ilogit(alpha_p) <- p_radier 
#p_radier~ dbeta(2, 2)
#p_pool <- ilogit(beta_p + alpha_p)


}"

# COMPILE the model  
nchains=3
nburnin=5000
niter=150000
nthin=10

cat( "Adapt...\n" )
model.fit <- jags.model( file = textConnection(mymodel),
                         data,
                         inits,
                         n.chains = nchains,
                         n.adapt = 1000 
)

cat( "Burnin...\n" )
update(model.fit, n.iter=nburnin)

# Save current states
#inits <- model.fit$state(internal=TRUE)

cat( "Sampling MCMC....\n" )
model.mcmc <- coda.samples(model.fit,
                           var = parameters,
                           n.iter = niter,
                           thin = nthin)

  
save(model.mcmc,file=paste0('results/MCMC_results_',espece,age,'.RData'))



# PLOT the posterior
p.prior=rbeta(15000,1,1)

rnd <-sample(1:data$N,3,replace = F)

tmp<-paste0("p[",rnd,"]")
MCMCtrace(model.mcmc, params = tmp, ISB = F, exact = T, 
          #priors = p.prior,
          pdf = FALSE, Rhat = TRUE, n.eff = TRUE)

#rnd <-sample(1:data$N,3,replace = F)

tmp<-paste0("dens[",rnd,"]")

MCMCtrace(model.mcmc, params = tmp, ISB = FALSE, exact = TRUE, 
           pdf = FALSE, Rhat = TRUE, n.eff = TRUE)


MCMCtrace(model.mcmc, params = "alpha", ISB = FALSE, exact = TRUE, 
          pdf = FALSE, Rhat = TRUE, n.eff = TRUE)


MCMCtrace(model.mcmc, params = "beta", ISB = FALSE, exact = TRUE, 
          pdf = FALSE, Rhat = TRUE, n.eff = TRUE)

MCMCtrace(model.mcmc, params = "gamma", ISB = FALSE, exact = TRUE, 
          pdf = FALSE, Rhat = TRUE, n.eff = TRUE)

MCMCtrace(model.mcmc, params = "p_radier", ISB = FALSE, exact = TRUE, 
          pdf = FALSE, Rhat = TRUE, n.eff = TRUE)

MCMCtrace(model.mcmc, params = "p_pool", ISB = FALSE, exact = TRUE, 
          pdf = FALSE, Rhat = TRUE, n.eff = TRUE)

#MCMCsummary(model.mcmc, params = parameters, round = 2)


mcmc <- as.matrix(model.mcmc)
ps <- mcmc[,paste0("p")]#[",1:data$N,"]")]
lambdas <- mcmc[,paste0("dens[",1:data$N,"]")]
probcap <- mcmc[,paste0("p[",1:data$N,"]")]

plot(ps,lambdas[,3],main = "Densit? en fonction de la probabilit? de capture pour ST0+")
plot(probcap[,3],lambdas[,3],main = "Densit? en fonction de la probabilit? de capture pour ST0+")

medians <- apply(lambdas,2, quantile, prob=0.5)#median
q1 <- apply(lambdas,2, quantile, prob=0.025)#quartile
q2 <- apply(lambdas,2, quantile, prob=0.25)#quartile
q3 <- apply(lambdas,2, quantile, prob=0.75)#quartile
q4 <- apply(lambdas,2, quantile, prob=0.975)#quartile


df$median <-  medians 
df$q1 <-  q1
df$q2 <-  q2
df$q3 <-  q3
df$q4 <-  q4

radier <- subset(df, Secteur %in% c("1", "2","3","5","6","7","8","10","11","12","13","15","16"))
pool <- subset(df, Secteur %in% c("P1","P2","P3","P4","P5","P6"))

## graphe densit?/date secteurs 1/16
plot(radier$Date,radier$median, 
     xlab = "Date (Ann?e de capture)", ylab = "Densit? (nomdre de poissons/m2)", 
     main = "Graphique de la densit? de Salmo trutta 0+ en fonction du secteur (1 ? 16) et de l'ann?e",
     col="blue", cex = 0,xaxt = "n", text(radier$Secteur),ylim = c(0,200))

segments (radier$Date,radier$q1, radier$Date, radier$q4 )
segments (radier$Date,radier$q2, radier$Date, radier$q3 ,lwd = 3, col = "grey")

text(radier$Date,radier$median,labels = radier$Secteur,col="blue")

at1<-seq(ISOdate(1977,1,1), ISOdate(2000,1,1), by = "1 year")
axis.Date(side=1, df$Date,  at=at1,las=2,  format= "%y")


## graphe densit?/date Pools
plot(pool$Date,pool$median, 
     xlab = "Date (Ann?e de capture)", ylab = "Densit? (nomdre de poissons/m2)", 
     main = "Graphique de la densit? de Salmo trutta 0+ en fonction du secteur (pool) et de l'ann?e",col="blue"
     ,cex = 0,xaxt = "n", text(pool$Secteur),ylim = c(0,200))

segments (pool$Date,pool$q1, pool$Date, pool$q4 )
segments (pool$Date,pool$q2, pool$Date, pool$q3 ,lwd = 3, col = "grey")


text(pool$Date,pool$median,labels = pool$Secteur,col="red")

at1<-seq(ISOdate(1977,1,1), ISOdate(2000,1,1), by = "1 year")
axis.Date(side=1, df$Date,  at=at1,las=2,  format= "%y")

## graphe secteur/date 1/16
plot(radier$Secteur,radier$median,
     xlab = "Secteurs", ylab = "Densit? (nomdre de poissons/m2)", 
     main = "Graphique de la densit? de Salmo trutta 2+ en fonction du secteur (1 ? 16) et de l'ann?e",
     col="blue", cex = 0,text(radier$Date),ylim = c(0,200),xaxt="n")

rs <-as.numeric(radier$Secteur)
segments (rs, radier$q1, rs, radier$q4 )
segments (rs, radier$q2, rs, radier$q3 ,lwd = 3, col = "grey")

colfunc<-colorRampPalette(c("red","yellow","springgreen","blue","darkblue"))
colors <- (colfunc(30))
colors <- colors[rank(radier$Date)]
text(rs,radier$median,labels = radier$year,col=colors)


## graphe secteur/date Pools

pool$num<- gsub("[P]"," ",pool$Secteur)

ps <-as.numeric(pool$num)



plot(ps,pool$median, 
     xlab = "Pool", ylab = "Densit? (nomdre de poissons/m2)", 
     main = "Graphique de la densit? de Salmo trutta 2+ en fonction du secteur et de l'ann?e",
     col="blue", cex = 0,text(pool$Date),ylim = c(0,230))


segments (ps, pool$q1, ps, pool$q4 )
segments (ps, pool$q2, ps, pool$q3 ,lwd = 3, col = "grey")

colfunc<-colorRampPalette(c("red","yellow","springgreen","darkblue"))
colors <- (colfunc(30))
colors <- colors[rank(pool$Date)]
text(ps,pool$median,labels = pool$year,col=colors)




