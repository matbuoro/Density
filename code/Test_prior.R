invlogit<-function(x) {1/(1+exp(-(x)))}
logit<-function(x) {log(x/(1-x))}
n=10000
delta <- rnorm(n,0, 1)
sigmaP <- rgamma(n, 2/5,5/2)#
sigma_eps <- rgamma(n, 2/5,5/2)#runif(n,0,1)
epsilonP1<-rnorm(n,0,sigmaP) 
epsilonP2<-rnorm(n,0,sigma_eps) 
log_muP <- delta + epsilonP1 + epsilonP2
hist(invlogit(log_muP))



kappa <-rlnorm(n,3,.5) # to keep 95% of kappa between [10; 50]
beta<-rlnorm(n,0,1)
alpha<-rlnorm(n,0,1)
t=1
muD <- kappa / (1+alpha * exp(-beta*(t)))
hist(muD)
sigmaD <- rgamma(n, 2/5,5/2)#runif(n,0,1)
dens<-rlnorm(n,log(muD), sigmaD)
hist(dens, breaks=250, xlim=c(0, 50))
quantile(dens,probs=c(0.05,0.5,0.95))


kappa=20
beta=.1
alpha=10
t=seq(0, 50, length.out=50)
muD <- kappa / (1+alpha * exp(-beta*(t)))
plot(muD~t, type="l", xlab="t", ylab="muD", main="muD vs t")