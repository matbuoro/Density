rm(list=ls())   # Clear memory
setwd( "C:/Dropbox/travail/Kerguelen/densité")

## Load libraries
library(rjags)
library(MCMCvis)
library(data.table)

###example with the 1+ dataset.
data1<-read.table("datajakdenyoung.txt",h=T)
data2<-read.table("datajakdenadult.txt",h=T)
data<-rbind(data1,data2)
table(data$basin)

##creating colonization vectors for any datasets (any of the two density datasets that is)  
#uncol=date jusqu'à laquelle certain pas de repro naturelle
uncolvector<-c(1983,1968,1978,1994,NA,1977,1984,2003,1985,1986,1986,1962,NA,1996,1985,1982,NA,1962,2000,1991,1998,1997,2004,NA,1981,1996,2017,1996,1982,1987,1986,1968,2014,2014,1986,1984,1997,2016,2004,1995,1988,1962,1979,1988,1996,1995,1987,1984,2004,2011) #still a problem here with gorfous
#col=reproduction=colonisé
colvector  <-c(1983,1968,1980,1994,NA,1977,1989,2007,1989,1992,1986,1962,NA,1996,1985,1990,NA,1962,2000,2000,2000,2000,2008,NA,1981,1996,2017,1999,1990,1987,1986,1968,2014,2014,1986,1984,1997,2016, NA ,2000,1992,1962,1979,1988,1996,1999,1987,1984, NA ,2013)

##pick the one you need. It should work for DL1/DL2 for now. And should be easily modifiable for PE or P1/P2. 
##loading 1+ dataset
data<-read.table("datajakdenyoung.txt",h=T)
## or loading 1+ dataset
data<-read.table("datajakdenyoung.txt",h=T)
data$year<-as.numeric(substr(data$date, 7, 10))  #crée variable $year. prend les caractères aux positions 7 à 10 pour l'année: 08.02.1970

#c() ouvre chaine de caractères
#vecteur uncoldate assigne année de colonisation pour chaque bassin versant
uncoldate<-rep("NA",length(data$year))
coldate<-rep("NA",length(data$year))

for (i in 1:length(data$year)) {
	if (data$basin[i]=="Acaena") {
		uncoldate[i]<-uncolvector[1]
		coldate[i]<-colvector[1]		
	} else if (data$basin[i]=="Albatros") {
		uncoldate[i]<-uncolvector[2]
		coldate[i]<-colvector[2]	
	} else if (data$basin[i]=="Americains") {
		uncoldate[i]<-uncolvector[3]
		coldate[i]<-colvector[3]
	} else if (data$basin[i]=="Armor") {
		uncoldate[i]<-uncolvector[4]
		coldate[i]<-colvector[4]
	} else if (data$basin[i]=="Azorella") {
		uncoldate[i]<-uncolvector[5]
		coldate[i]<-colvector[5]		
	} else if (data$basin[i]=="Borgne") {
		uncoldate[i]<-uncolvector[6]
		coldate[i]<-colvector[6]
	} else if (data$basin[i]=="Bungay") {
		uncoldate[i]<-uncolvector[7]
		coldate[i]<-colvector[7]		
	} else if (data$basin[i]=="Calcedoines") {
		uncoldate[i]<-uncolvector[8]
		coldate[i]<-colvector[8]		
	} else if (data$basin[i]=="Cataractes") {
		uncoldate[i]<-uncolvector[9]
		coldate[i]<-colvector[9]		
	} else if (data$basin[i]=="Charbon") {
		uncoldate[i]<-uncolvector[10]
		coldate[i]<-colvector[10]
	} else if (data$basin[i]=="Chasseurs") {
		uncoldate[i]<-uncolvector[11]
		coldate[i]<-colvector[11]
	} else if (data$basin[i]=="Chateau") {
		uncoldate[i]<-uncolvector[12]
		coldate[i]<-colvector[12]
	} else if (data$basin[i]=="Chateau_>_Ferme_ou_Etangs") {
		uncoldate[i]<-uncolvector[13]
		coldate[i]<-colvector[13]				
	} else if (data$basin[i]=="Claree") {
		uncoldate[i]<-uncolvector[14]
		coldate[i]<-colvector[14]			
	} else if (data$basin[i]=="Doute") {
		uncoldate[i]<-uncolvector[15]
		coldate[i]<-colvector[15]			
	} else if (data$basin[i]=="Est") {
		uncoldate[i]<-uncolvector[16]
		coldate[i]<-colvector[16]
	} else if (data$basin[i]=="Etang_des_Beliers") {
		uncoldate[i]<-uncolvector[17]
		coldate[i]<-colvector[17]				
	} else if (data$basin[i]=="Ferme") {
		uncoldate[i]<-uncolvector[18]
		coldate[i]<-colvector[18]
	} else if (data$basin[i]=="Gorfous_1") {
		uncoldate[i]<-uncolvector[19]
		coldate[i]<-colvector[19]		
	} else if (data$basin[i]=="Gorfous_2") {
		uncoldate[i]<-uncolvector[20]
		coldate[i]<-colvector[20]
	} else if (data$basin[i]=="Gorfous_4") {
		uncoldate[i]<-uncolvector[21]
		coldate[i]<-colvector[21]
	} else if (data$basin[i]=="Gorfous_5") {
		uncoldate[i]<-uncolvector[22]
		coldate[i]<-colvector[22]
	} else if (data$basin[i]=="Grisanche") {
		uncoldate[i]<-uncolvector[23]
		coldate[i]<-colvector[23]
	} else if (data$basin[i]=="Isthme_du_Lac") {
		uncoldate[i]<-uncolvector[24]
		coldate[i]<-colvector[24]		
	} else if (data$basin[i]=="Korrigans") {
		uncoldate[i]<-uncolvector[25]
		coldate[i]<-colvector[25]
	} else if (data$basin[i]=="Levant") {
		uncoldate[i]<-uncolvector[26]
		coldate[i]<-colvector[26]
	} else if (data$basin[i]=="Lozère") {
		uncoldate[i]<-uncolvector[27]
		coldate[i]<-colvector[27]				
	} else if (data$basin[i]=="Macaronis") {
		uncoldate[i]<-uncolvector[28]
		coldate[i]<-colvector[28]
	} else if (data$basin[i]=="Manchots") {
		uncoldate[i]<-uncolvector[29]
		coldate[i]<-colvector[29]
	} else if (data$basin[i]=="Mouettes") {
		uncoldate[i]<-uncolvector[30]
		coldate[i]<-colvector[30]
	} else if (data$basin[i]=="Nord") {
		uncoldate[i]<-uncolvector[31]
		coldate[i]<-colvector[31]
	} else if (data$basin[i]=="Norvegienne") {
		uncoldate[i]<-uncolvector[32]
		coldate[i]<-colvector[32]
	} else if (data$basin[i]=="Olsen") {
		uncoldate[i]<-uncolvector[33]
		coldate[i]<-colvector[33]
	} else if (data$basin[i]=="Orgues") {
		uncoldate[i]<-uncolvector[34]
		coldate[i]<-colvector[34]
	} else if (data$basin[i]=="Pepins") {
		uncoldate[i]<-uncolvector[35]
		coldate[i]<-colvector[35]
	} else if (data$basin[i]=="Planchette") {
		uncoldate[i]<-uncolvector[36]
		coldate[i]<-colvector[36]
	} else if (data$basin[i]=="Port-Kirk") {
		uncoldate[i]<-uncolvector[37]
		coldate[i]<-colvector[37]		
	} else if (data$basin[i]=="Radioleine") {
		uncoldate[i]<-uncolvector[38]
		coldate[i]<-colvector[38]
	} else if (data$basin[i]=="Ravin_du_Charbon") {
		uncoldate[i]<-uncolvector[39]
		coldate[i]<-colvector[39]				
	} else if (data$basin[i]=="Rohan") {
		uncoldate[i]<-uncolvector[40]
		coldate[i]<-colvector[40]
	} else if (data$basin[i]=="Serail") {
		uncoldate[i]<-uncolvector[41]
		coldate[i]<-colvector[41]
	} else if (data$basin[i]=="Studer") {
		uncoldate[i]<-uncolvector[42]
		coldate[i]<-colvector[42]
	} else if (data$basin[i]=="Sud") {
		uncoldate[i]<-uncolvector[43]
		coldate[i]<-colvector[43]
	} else if (data$basin[i]=="Trois_Lacs") {
		uncoldate[i]<-uncolvector[44]
		coldate[i]<-colvector[44]
	} else if (data$basin[i]=="Val-Travers") {
		uncoldate[i]<-uncolvector[45]
		coldate[i]<-colvector[45]
	} else if (data$basin[i]=="Val_d'Auge") {
		uncoldate[i]<-uncolvector[46]
		coldate[i]<-colvector[46]
	} else if (data$basin[i]=="Val_de_l'Ouest") {
		uncoldate[i]<-uncolvector[47]
		coldate[i]<-colvector[47]
	} else if (data$basin[i]=="Val_Raide") {
		uncoldate[i]<-uncolvector[48]
		coldate[i]<-colvector[48]
	} else if (data$basin[i]=="Valdotaine") {
		uncoldate[i]<-uncolvector[49]
		coldate[i]<-colvector[49]		
	} else if (data$basin[i]=="Vallee_des_Merveilles") {
		uncoldate[i]<-uncolvector[50]
		coldate[i]<-colvector[50]
	}
}

data$coldate<-as.numeric(coldate)
data$uncoldate<-as.numeric(uncoldate)
doubtDate<-(data$coldate)-(data$uncoldate) # the span of colonization date uncertainty. We may want to draw into this if we are to account for that source of variation. 

#here we define the age of sampling with regard to the population age. 
data$popAge<-as.numeric(data$year)-as.numeric(data$coldate)


#here we filter to obtain only sites that were sampled in DL1 / DL2. 
#nouveau dataset dans les NA
data<-subset(data, !is.na(DL1) & !(is.na(DL2)) ) #garde seulement les deux passages

#note that these IDs are not the same between different datasets: if we want to merge resutls, we will have to use basin names. 
data$riverID<-unclass(factor(data$basin)) #unclass prend le rang de chaque catégorie. Basin transformée en facteur: variable catégorielle
data$siteID<-rowid(data$XYZ,factor(data$riverID))  # this is the sampling site ID WITHIN the riverID. Useful for hierarchization. #rowid rang à l'intérieur d'un facteur, pour site dans rivière



## here we place the data in a format needed for Jags. 

database <- list(                                               #liste aggrégée d'objets
  N = nrow(data),                                               #N fait n de long avec répétitions NA
  Nriver= length(table(data$riverID)),
  D1 = data$DL1,						#D1 issu de proba de capture binomial
  D2 = data$DL2,
  n = rep(NA,nrow(data)), 
  area = (as.numeric(data$area)),  # we work in log here
  riverID = data$riverID,
  siteID = data$siteID,  
  year=data$year,   # it is the year of sampling
  popAge=data$popAge  # it is the population age. 
)





# DEFINE the model: already evolved model.
mymodel <- "
model{

for (i in 1:N){
	## Likelihood 
	n[i] ~ dpois(lambda[i]*area[i]) # n: nbr poisson, area: surface en m2, lambda : nb poisson/m2
	D1[i] ~ dbin(p[i], n[i]) # capture 1er passage
	D2[i] ~ dbin(p[i], n[i]-D1[i]) # capture 2eme passage

	# proba capture
	logit(p[i]) <- alpha_p + epsilon[riverID[i]]  #we start with the assumption that there is a local river effect on capture proba. maybe not useful. 

	# likelihood for  the area. NB: not clear trend as a function of time or population age so don't bother. Like wise for river effect. 
	area[i] ~ dlnorm(alpha_area,tau_area)
	#log(area[i]) <- logarea[i]
	#logarea[i] ~ dnorm(alpha_area,tau_area)  # same process for all rivers/sites. 

	# densite: log effect on time: must think about this !
	log(lambda[i]) <- loglambda[i]
	loglambda[i] ~ dnorm(mu[i],tau_lambda)
	mu[i] <- alpha_mu +  beta_mu * log(popAge[i]+3) + eta[riverID[i]]  # + 3 just to avoid zero problem for now. But it deserves some thinking. Visitors really? or uncertainty ?
}
# Random effect river
for (i in 1:Nriver){
	epsilon[i]~dnorm(0,tau_epsilon) #random effect for capture probability
	eta[i] ~ dnorm(0,tau_eta)	#random effect for river density. 
}
##PRIORS
# coef regression proba capture
alpha_p ~ dnorm(0,0.1)   

# coef regression densite
alpha_mu ~ dnorm(0,0.1)
beta_mu ~ dnorm(0,0.1)
alpha_area ~ dnorm(0,0.01)

# precision
tau_epsilon <- 1/(sigma_epsilon*sigma_epsilon)
sigma_epsilon ~ dunif(0,10) #prior sur l'écart type 
tau_eta <- 1/(sigma_eta*sigma_eta)
sigma_eta ~ dunif(0,10)
tau_lambda <- 1/(sigma_lambda*sigma_lambda)
sigma_lambda ~ dunif(0,10)
tau_area <- 1/(sigma_area*sigma_area)
sigma_area ~ dunif(0,10)
## end of likelihood
}"



##providing inits for areas. or else invalid parent node (since they are related to n). 
truc<-rep(NA, length(database$area))
truc[is.na(database$area)]<- 250 

## Initial values for unknown parameters (prior) 
inits = function(){list(
  alpha_mu=runif(1,0.4,0.7)
  ,beta_mu =runif(1,-0.1,0.1)
  ,alpha_p = runif(1,0,1)  
  ,sigma_epsilon = runif(1,0.2,0.5)
  ,sigma_lambda = runif(1,0.6,1)
  ,sigma_eta = runif(1,0.6,1)
  ,sigma_area = runif(1,0.6,1)
  ,n = (data$DL1+data$DL2+25)    ## here we provide inits for unobserved data or else, invalid parent value...
  ,area = truc
)}


## Parameters to monitor
parameters <- c("alpha_p"
                ,"epsilon"
                ,"alpha_mu","beta_mu","alpha_area"
                ,"sigma_epsilon","sigma_eta","sigma_lambda","sigma_area"
                ,"mu", "n","p"
)







###Now we run the model

# COMPILE the model  
nchains=3
nburnin=500 # 5000 is best
niter=10000 # 5000 is best
nthin=10

INITS <- list(inits(),inits(),inits())
#load("inits/INITS_ST1.RData")
cat( "Adapt...\n" )
model.fit <- jags.model( file = textConnection(mymodel),
                         database,
                         INITS,
                         n.chains = nchains,
                         n.adapt = 1000 
)

#INITS = model.fit$state()
#save(INITS,file=paste0('results/INITS_',espece,age,'.RData'))

cat( "Burnin...\n" )
update(model.fit, n.ite=nburnin)


cat( "Sampling MCMC....\n" )
model.mcmc <- coda.samples(model.fit,
                           var = parameters,
                           n.iter = niter,
                           thin = nthin)


###########  results

MCMCtrace(model.mcmc, params = "sigma_lambda", ISB = FALSE, exact = TRUE, pdf = FALSE, Rhat = TRUE, n.eff = TRUE)
MCMCtrace(model.mcmc, params = "sigma_eta", ISB = FALSE, exact = TRUE, pdf = FALSE, Rhat = TRUE, n.eff = TRUE)
MCMCtrace(model.mcmc, params = "sigma_epsilon", ISB = FALSE, exact = TRUE, pdf = FALSE, Rhat = TRUE, n.eff = TRUE)
MCMCtrace(model.mcmc, params = "alpha_p", ISB = FALSE, exact = TRUE, pdf = FALSE, Rhat = TRUE, n.eff = TRUE)
MCMCtrace(model.mcmc, params = "alpha_mu", ISB = FALSE, exact = TRUE, pdf = FALSE, Rhat = TRUE, n.eff = TRUE)
MCMCtrace(model.mcmc, params = "beta_mu", ISB = FALSE, exact = TRUE, pdf = FALSE, Rhat = TRUE, n.eff = TRUE)
MCMCtrace(model.mcmc, params = "alpha_area", ISB = FALSE, exact = TRUE, pdf = FALSE, Rhat = TRUE, n.eff = TRUE)


MCMCtrace(model.mcmc, params = paste0("p[",1,"]"), ISB = FALSE, exact = TRUE, pdf = FALSE, Rhat = TRUE, n.eff = TRUE)

MCMCplot(model.mcmc, params = paste0("p[",1,"]"), ISB = FALSE, exact = TRUE)

MCMCplot(model.mcmc, params = paste0("mu[",1:715,"]"), ISB = FALSE, exact = TRUE)
MCMCplot(model.mcmc, params = paste0("mu[",1:100,"]"), ISB = FALSE, exact = TRUE)
MCMCplot(model.mcmc, params = paste0("p[",1:715,"]"), ISB = FALSE, exact = TRUE)
MCMCplot(model.mcmc, params = paste0("n[",1:715,"]"), ISB = FALSE, exact = TRUE)



MCMCplot(model.mcmc, params = paste0("epsilon[",1:32,"]"), ISB = FALSE, exact = TRUE)
