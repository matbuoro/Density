

rm(list = ls())

setwd( "C:/Dropbox/travail/Kerguelen/densité/git") # ton chemin.


data<-read.table("data/census_data.txt",h=T, fill=TRUE)

data <- subset(data, data$basin != "Chateau_>_Ferme_ou_Etangs")

table(data$basin)

length(unique((data$basin)))

# Remove area too large
data$area <- gsub(",", ".", data$area) # convert comma to dot
data$area <- as.numeric(data$area) # convert to numeric

#Include estimated areas in column area
data$estArea<-as.numeric(as.character(data$estArea))
hist(data$area)
hist(data$estArea)
for (i in 1:length(data$area)){
  if (is.na(data$area[i])){
    data$area[i]<-data$estArea[i]
  }
  
}

data <- data[-which(data$area>2500),] # remove area above 2500m2
# comparer distributions 




data <- subset(data, data$basin != "Isthme_du_Lac")
data <- subset(data, data$basin != "Etang_des_Beliers")
data <- subset(data, data$basin != "Ampere")
###we should remove ravin du charbon. ?
data <- subset(data, data$basin != "Ravin_du_Charbon")




##creating colonization vectors for any datasets (any of the two density datasets that is)  
#uncol=date jusqu'? laquelle certain pas de repro naturelle

uncolvector<-c(1983,1968,1978,1994,1997,1977,1984,2003,2020,1985,1986,1986,1962,1996,1985,2020,2020,1982,1962,2000,1991,1998,1997,2004,1981,2015,1996,2017,1996,1982,2011,2017,1987,1986,1968,2014,2014,1989,1984,1997,2016,2004,1995,1988,1962,1979,1989,1996,1995,1986,1987,2004) #still a problem here with gorfous
colvector  <-c(1983,1968,1980,1994,1998,1977,1989,2007,2023,1989,1992,1986,1962,1996,1985,2022,2022,1990,1962,2000,2000,2000,2000,2008,1981,2020,1996,2017,1999,1990,2014,2023,1987,1986,1968,2014,2014,1989,1984,1997,2016,2020,2000,1992,1962,1979,1989,1996,1999,1986,1987,2012)
# removing RAvin du CHarbon
uncolvector<-uncolvector[-42]
colvector<-colvector[-42]              




##Get years and month
data$year<-as.numeric(substr(data$date, 7, 10))  
data$month<-as.numeric(substr(data$date, 4, 5)) 
#mydates = as.POSIXlt(data$date, format = c("%d-%m-%Y"))
#Norvegienne$year<-as.numeric(format(as.Date(mydates), "%Y"))
#data$month<-as.numeric(format(as.Date(mydates), "%m"))
#df$day<-as.numeric(format(as.Date(mydates), "%d"))
#data$jday<-mydates$yday # julian day


aggregate(PE~year+basin, data=data, FUN=length)


uncoldate<-NULL#rep(,nrow(data))
coldate<-NULL#rep(,nrow(data))

for (i in 1:nrow(data)) {
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
	} else if (data$basin[i]=="Casque") {
	  uncoldate[i]<-uncolvector[9]
	  coldate[i]<-colvector[9]	
	} else if (data$basin[i]=="Cataractes") {
		uncoldate[i]<-uncolvector[10]
		coldate[i]<-colvector[10]		
	} else if (data$basin[i]=="Charbon") {
		uncoldate[i]<-uncolvector[11]
		coldate[i]<-colvector[11]
	} else if (data$basin[i]=="Chasseurs") {
		uncoldate[i]<-uncolvector[12]
		coldate[i]<-colvector[12]
	} else if (data$basin[i]=="Chateau") {
		uncoldate[i]<-uncolvector[13]
		coldate[i]<-colvector[13]
	} else if (data$basin[i]=="Claree") {
		uncoldate[i]<-uncolvector[14]
		coldate[i]<-colvector[14]			
	} else if (data$basin[i]=="Doute") {
		uncoldate[i]<-uncolvector[15]
		coldate[i]<-colvector[15]	
	} else if (data$basin[i]=="Entrelacs") {
	  uncoldate[i]<-uncolvector[16]
	  coldate[i]<-colvector[16]
	} else if (data$basin[i]=="Est") {	##was mixed with Euphrosine.
	  uncoldate[i]<-uncolvector[17]
	  coldate[i]<-colvector[17]
	} else if (data$basin[i]=="Euphrosine") {
		uncoldate[i]<-uncolvector[18]
		coldate[i]<-colvector[18]
	} else if (data$basin[i]=="Ferme") {
		uncoldate[i]<-uncolvector[19]
		coldate[i]<-colvector[19]
	} else if (data$basin[i]=="Gorfous_1") {
		uncoldate[i]<-uncolvector[20]
		coldate[i]<-colvector[20]		
	} else if (data$basin[i]=="Gorfous_2") {
		uncoldate[i]<-uncolvector[21]
		coldate[i]<-colvector[21]
	} else if (data$basin[i]=="Gorfous_4") {
		uncoldate[i]<-uncolvector[22]
		coldate[i]<-colvector[22]
	} else if (data$basin[i]=="Gorfous_5") {
		uncoldate[i]<-uncolvector[23]
		coldate[i]<-colvector[23]
	} else if (data$basin[i]=="Grisanche") {
		uncoldate[i]<-uncolvector[24]
		coldate[i]<-colvector[24]
	} else if (data$basin[i]=="Korrigans") {
		uncoldate[i]<-uncolvector[25]
		coldate[i]<-colvector[25]
	} else if (data$basin[i]=="Larmor") {
	  uncoldate[i]<-uncolvector[26]
	  coldate[i]<-colvector[26]
	} else if (data$basin[i]=="Levant") {
		uncoldate[i]<-uncolvector[27]
		coldate[i]<-colvector[27]
	} else if (data$basin[i]=="Lozere") {
		uncoldate[i]<-uncolvector[28]
		coldate[i]<-colvector[28]				
	} else if (data$basin[i]=="Macaronis") {
		uncoldate[i]<-uncolvector[29]
		coldate[i]<-colvector[29]
	} else if (data$basin[i]=="Manchots") {
		uncoldate[i]<-uncolvector[30]
		coldate[i]<-colvector[30]
	} else if (data$basin[i]=="Merveilles") {
	  uncoldate[i]<-uncolvector[31]
	  coldate[i]<-colvector[31]
	} else if (data$basin[i]=="Mouche") {
	  uncoldate[i]<-uncolvector[32]
	  coldate[i]<-colvector[32]
	} else if (data$basin[i]=="Mouettes") {
		uncoldate[i]<-uncolvector[33]
		coldate[i]<-colvector[33]
	} else if (data$basin[i]=="Nord") {
		uncoldate[i]<-uncolvector[34]
		coldate[i]<-colvector[34]
	} else if (data$basin[i]=="Norvegienne") {
		uncoldate[i]<-uncolvector[35]
		coldate[i]<-colvector[35]
	} else if (data$basin[i]=="Olsen") {
		uncoldate[i]<-uncolvector[36]
		coldate[i]<-colvector[36]
	} else if (data$basin[i]=="Orgues") {
		uncoldate[i]<-uncolvector[37]
		coldate[i]<-colvector[37]
	} else if (data$basin[i]=="Pepins") {
		uncoldate[i]<-uncolvector[38]
		coldate[i]<-colvector[38]
	} else if (data$basin[i]=="Planchette") {
		uncoldate[i]<-uncolvector[39]
		coldate[i]<-colvector[39]
	} else if (data$basin[i]=="Port-Kirk") {
		uncoldate[i]<-uncolvector[40]
		coldate[i]<-colvector[40]		
	} else if (data$basin[i]=="Radioleine") {
		uncoldate[i]<-uncolvector[41]
		coldate[i]<-colvector[41]
	#} else if (data$basin[i]=="Ravin_du_Charbon") {
	#	uncoldate[i]<-uncolvector[42]
	#	coldate[i]<-colvector[42]				
	} else if (data$basin[i]=="Rohan") {
		uncoldate[i]<-uncolvector[42]
		coldate[i]<-colvector[42]
	} else if (data$basin[i]=="Serail") {
		uncoldate[i]<-uncolvector[43]
		coldate[i]<-colvector[43]
	} else if (data$basin[i]=="Studer") {
		uncoldate[i]<-uncolvector[44]
		coldate[i]<-colvector[44]
	} else if (data$basin[i]=="Sud") {
		uncoldate[i]<-uncolvector[45]
		coldate[i]<-colvector[45]
	} else if (data$basin[i]=="Trois_Lacs") {
		uncoldate[i]<-uncolvector[46]
		coldate[i]<-colvector[46]
	} else if (data$basin[i]=="Val-Travers") {
		uncoldate[i]<-uncolvector[47]
		coldate[i]<-colvector[47]
	} else if (data$basin[i]=="Val_d_Auge") {
		uncoldate[i]<-uncolvector[48]
		coldate[i]<-colvector[48]
	} else if (data$basin[i]=="Val_de_l_Ouest") {
		uncoldate[i]<-uncolvector[49]
		coldate[i]<-colvector[49]
	} else if (data$basin[i]=="Val_Raide") {
		uncoldate[i]<-uncolvector[50]
		coldate[i]<-colvector[50]
	} else if (data$basin[i]=="Valdotaine") {
		uncoldate[i]<-uncolvector[51]
		coldate[i]<-colvector[51]		
	}
}

data$coldate<-as.numeric(coldate)
data$uncoldate<-as.numeric(uncoldate)
doubtDate<-(data$coldate)-(data$uncoldate) # the span of colonization date uncertainty. We may want to draw into this if we are to account for that source of variation. 
#cherche les lignes qui ont NA dans coldate
data$basin[is.na(data$coldate)]

#here we define the age of sampling with regard to the population age. 
data$popAge<-(data$year)-(data$coldate)
#data <- data[-which(data$popAge<0),] # remove colDate > observation
data <- data[-which(data$popAge<(-3)),] # remove colDate > observation  ##risky business. Remove some rivers -> mess with IDs numbers ?


# Keep only data from december to February
#data <- subset(data, month %in% c(12,1,2,3))


# Cohort year: les captures de decembre sont attribuées à l'année suivante
data$Year_code <- data$year-min(data$year)+1 # recode year
#data$Year_cohort <- ifelse(data$month ==12, data$Year_code+1, data$Year_code)
#data$Age_cohort <- ifelse(data$month ==12, data$popAge+1, data$popAge)
data$Year_cohort <- ifelse(data$month > 6, data$Year_code+1, data$Year_code)
data$Age_cohort <- ifelse(data$month > 6, data$popAge+1, data$popAge)

#here we filter to obtain only sites that were sampled in DL1 / DL2 or Petersen. Or PE.
data<-subset(data, (!is.na(DL1) & !(is.na(DL2))) | ((!is.na(P1)) & !(is.na(P2))) | (!is.na(PE))) 

## here we place the data in a format needed for Jags. 

#note that these IDs are not the same between different datasets: if we want to merge results, we will have to use basin names
data$riverID<-unclass(factor(data$basin)) #unclass prend le rang de chaque categorie. Basin transforme en facteur: variable categorielle
#data$siteID<-rowid(data$XYZ,factor(data$riverID))  # this is the sampling site ID WITHIN the riverID. Useful for hierarchization
# Retrieve levels of the factor
factor_levels <- levels(factor(data$basin))
# Recode factor based on position
recode_factor <- factor(factor(data$basin), labels = seq_along(factor_levels))

maxPopAge=firstCapture=NULL
trueMaxPopAge<-2025-colvector

for (pop in 1:max(data$riverID)){
maxPopAge[pop] <- max(data$Age_cohort[recode_factor==pop]) ## surely this is right but yet...
firstCapture[pop] <- min(data$Year_cohort[recode_factor==pop])
}

year_capture=NULL
for (i in 1:nrow(data)){
 data$year_capture[i]=data$Year_cohort[i] - firstCapture[data$riverID[i]]
}


dataToJags <- list(                                               #liste aggr?g?e d'objets
  n = nrow(data),                                              #N fait n de long avec r?p?titions NA
  #Nriver= length(unique(data$riverID)),
  DL1 = data$DL1,						
  DL2 = data$DL2,
  #DL3 = data$DL3,
  P1 = data$P1, 
  P2 = data$P2, 
  PE = data$PE, # PE
  #n = rep(NA,nrow(data)), 
  area = data$area,  # we work in log here
  riverID = as.integer(recode_factor),
  #vec_riverID=unique(data$riverID),
  #siteID = data$siteID,  
  year= data$year - min(data$year)+1,   # it is the year of sampling
  popAge=data$Age_cohort,  # it is the population age /!\ but by cohort gb+mb 27032024
  #max_year = max(data$Year_cohort)
  year_capture=data$year_capture,
  t0= doubtDate+1,
  coldate=data$coldate,
  maxPopAge=maxPopAge,
  trueMaxPopAge=trueMaxPopAge)




