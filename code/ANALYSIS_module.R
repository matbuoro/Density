rm(list = ls())

# #!/usr/bin/env Rscript
# args <- commandArgs(TRUE)
# print(args)
# if (length(args)==0){
#   args <- ""
# }


# PACKAGES ####
library(R2jags)
library(doParallel)
#library(MASS)
#library(fst)

# DIRECTORY ####
#dir <- "/media/hdd12To/mbuoro/TimingMigration/"
#setwd(dir)

# FUNCTIONS ####
invlogit<-function(x) {1/(1+exp(-(x)))}
logit<-function(x) {log(x/(1-x))}


## MODEL ####
##2. Modelisation statistique: inférence des paramètres en fonction des données 
#langage bugs
# source("code/MODEL_density_BH.R")
#source("code/MODEL_density_log.R")


## DATA ####
source("code/DATA_format.R")
attach(dataToJags)


#load results from the naive model , De Lury only
#mcmc_CMR <- readRDS("results/mcmc_CMR.rds")
load("results/jagsfit_naive_DL.Rdata")

# Step 1: Extract the MCMC samples from the jagsfit object
mcmc <- jagsfit$BUGSoutput$sims.matrix

## Step 2: Select iteration and posterior values to keep ####
step=200 # thining
iter=seq(1000,nrow(mcmc), by=step) # retained iterations
niter=length(iter)
print(niter)
try(if(niter < 50) stop("not enough iterations"))

mcmc <- mcmc[iter,] # keep only the selected iterations

# Step 3: Identify columns that contain 'P_pred'
# Alternatively, using startsWith
P_columns <- colnames(mcmc)[startsWith(colnames(mcmc), "P_pred")]

# Step 4: Subset the data frame to extract all p_trap2
P <- mcmc[, P_columns]

# Step 5: Convert the data frame to a matrix
p <- as.matrix(P)



## Step 6: Store de datasets for each iteration ####
data_iter=list()
for (i in 1:niter){
  data_iter[[i]] <- dataToJags
  data_iter[[i]]$p <- p[i,]
}



## INITS ####
area_inits<-dataToJags$area
area_inits[is.na(area_inits)]<- 250 
#dataToJags$area <- area_inits

N_inits=dens=NULL
for (i in 1:dataToJags$n){
  #for (i in 1:900){
  N_inits[i] <- ceiling(sum(c(dataToJags$DL1[i],dataToJags$DL2[i],dataToJags$DL3[i]
                              ,dataToJags$P1[i],dataToJags$P2[i],dataToJags$PE[i]
  ), na.rm = TRUE)/0.7)
  
  dens[i]<-N_inits[i]/(area_inits[i]/100)
}

area <- dataToJags$area
area_inits<-rep(NA, length(area))
area_inits[is.na(area)]<- 250 
#area_inits <- area_inits[1:dataToJags$n1[2]]


inits<-function(){ # works for naive
  list(
    #n1=dataToJags$n1
    N=N_inits
    , dens=dens
    #, delta=invlogit(0.7)
    #, sigmaP=0.5
    #, sigma_eps=0.5
    , sigmaD=1
    , kappa=exp(3.5)
    #, pre_kappa=0.5
    #, sigma_kappa=0.1
    , alpha=10
    , beta=0.2
    , area=area_inits
    #, r=10
  )
}

parameters <-c("muD"
               ,"muD_pred"
               ,"kappa"
               #,"mu_kappa","sigma_kappa"
               ,"alpha"
               ,"beta"
               #,"pmoy"
               #,"P_pred"
               #,"sigma_eps"
               ,"sigmaD"
               #,"delta"
               #,"sigmaP"
               ,'muS',"sigmaS"
               ,"area","nu","sigma_nu"
               #,"r"
               ,"C_pred"
               ,"t50"
) 





# SIMULATIONS ####
start_time <- Sys.time()

cl <- makeCluster(niter)#, outfile="log.txt")
registerDoParallel(cl)

results <- foreach(i=1:niter, .packages='R2jags', .verbose=T) %dopar% {
  
  tryCatch({
    
    
    ## Functions ####
    #source("code/1_Functions.R")
    
    # MODEL ####
    source("code/MODEL_density_module.R")
    
    ## Data ####
    data=NULL
    data <- data_iter[[i]]
    
    ## Run ####
    output <- jags(data,  
                    model.file = modelstat,
                    parameters.to.save = parameters,  
                    n.chains = 2,  # Number of chains to run.
                    inits = inits,  # initial values for hyperparameters
                    n.iter = 200*1,   #MCMC iterations, ajouter si converge pas
                    n.burnin = 100,   # discard first X iterations
                    n.thin = 1
    ) # keep every X iterations //ex: garde tous les 100 itérations
    
  }, error = function(e) return(paste0("The iteration '", i, "'",  " caused the error: '", e, "'")))
  #cat(dput(i), file = paste0("debug_file_", i, ".txt"))
}

end_time <- Sys.time()
time_run <- end_time - start_time
print(time_run)

# Cleaning
stopCluster(cl)
rm(cl)


# RESULTS ####
save(results,file=paste0("results/MCMC_module.Rdata"))
