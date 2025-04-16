
q=0.5
beta=.1
d=NULL
for (i in 1:100){
  d[i] <- log(1-q)/(log(q) + log(beta) -log(i))
}


a=8
b=3
d=1
dens=NULL
for (t in 1:25){
  dens[t] <- (a*(t^d))/(b^d + t^d)
}
plot(1:25,dens,type="l")






beta<-0.001
tq<-runif(100,1,20)
q<-0.25
d <- log(1-q)/(log(q) + log(beta) -log(tq))
hist(d)


mean(rexp(100,1))
