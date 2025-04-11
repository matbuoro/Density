
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

data$is.there.area <- ifelse(is.na(data$area), "No", "Yes")


data <- subset(data, data$basin != "Isthme_du_Lac")
data <- subset(data, data$basin != "Etang_des_Beliers")
data <- subset(data, data$basin != "Ampere")

data <- subset(data, data$basin != "Ravin_du_Charbon")




##creating colonization vectors for any datasets (any of the two density datasets that is)  
#uncol=date jusqu'? laquelle certain pas de repro naturelle

uncolvector<-c(1983,1968,1978,1994,1997,1977,1984,2003,2020,1985,1986,1986,1962,1996,1985,2020,1982,2020,1962,2000,1991,1998,1997,2004,1981,2015,1996,2017,1996,1982,2011,2017,1987,1986,1968,2014,2014,1989,1984,1997,2016,2004,1995,1988,1962,1979,1989,1996,1995,1986,1987,2004) #still a problem here with gorfous
colvector  <-c(1983,1968,1980,1994,1998,1977,1989,2007,2023,1989,1992,1986,1962,1996,1985,2022,1990,2022,1962,2000,2000,2000,2000,2008,1981,2020,1996,2017,1999,1990,2014,2023,1987,1986,1968,2014,2014,1989,1984,1997,2016,2020,2000,1992,1962,1979,1989,1996,1999,1986,1987,2012)
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
	} else if (data$basin[i]=="Est") {
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
	# } else if (data$basin[i]=="Ravin_du_Charbon") {
	# 	uncoldate[i]<-uncolvector[42]
	# 	coldate[i]<-colvector[42]				
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

data$metapopAge<-data$year-min(coldate)
data$popAge<-(data$year)-(data$coldate)

data <- data[which(data$popAge>0),] # remove colDate > observation
#data <- data[-which(data$popAge<(-3)),] # remove colDate > observation  ##risky business. Remove some rivers -> mess with IDs numbers ?

# #Take out areas considered not reliable
table((as.factor(data$comment))=="not_reliable")
data <- subset(data, comment != "not_reliable" & comment != "unreliable" | is.na(comment))  #exclut 244 lignes

# Keep only data from december to February
# data <- subset(data, month %in% c(12,1,2,3))


# # Cohort year: les captures de decembre sont attribuées à l'année suivante
# data$Year_code <- data$year-min(data$year)+1 # recode year
# # data$Year_cohort <- ifelse(data$month ==12, data$Year_code+1, data$Year_code)
# # data$Age_cohort <- ifelse(data$month ==12, data$popAge+1, data$popAge)
# data$Year_cohort <- ifelse(data$month > 6, data$Year_code+1, data$Year_code)
# data$Age_cohort <- ifelse(data$month > 6, data$popAge+1, data$popAge)


#here we filter to obtain only sites that were sampled in DL1 / DL2 or Petersen. 
data<-subset(data, (!is.na(DL1) & !(is.na(DL2))) | ((!is.na(P1)) & !(is.na(P2))) | (!is.na(PE))) 

#note that these IDs are not the same between different datasets: if we want to merge results, we will have to use basin names
data$riverID<-unclass(factor(data$basin)) #unclass prend le rang de chaque categorie. Basin transforme en facteur: variable categorielle
#data$siteID<-rowid(data$XYZ,factor(data$riverID))  # this is the sampling site ID WITHIN the riverID. Useful for hierarchization

basin_vector <- levels(factor(data$basin))
# Recode factor based on position
recode_factor <- factor(factor(data$basin), labels = seq_along(basin_vector))

maxPopAge=firstCapture=NULL
trueMaxPopAge<-2025-colvector

#relatif à la cohorte donc ne va que jusque dernière pèche et pas jusque 2025. Ex: 
for (pop in 1:max(data$riverID)){
maxPopAge[pop] <- max(data$popAge[recode_factor==pop])+ (2025-max(data$year[recode_factor==pop]))
firstCapture[pop] <- min(data$popAge[recode_factor==pop])
}

# year_capture=NULL
# for (i in 1:nrow(data)){
#  data$year_capture[i]=data$Year_cohort[i] - firstCapture[data$riverID[i]]
# }

#Censustype
data$censusType <- ifelse(!is.na(data$DL1), 1, 
                          ifelse(!is.na(data$P1), 2, 
                                 ifelse(!is.na(data$PE), 3, NA)))

# remove area not available
#data <- data[which(data$is.there.area=="Yes"),]

# Sorting the data frame by censusType, riverID, and year
data <- data[order(data$censusType, data$riverID, data$year), ]

# Function to extract the first and last row of each group
get_first_last <- function(x) {
  x[c(1, nrow(x)), ]
}

# Get the row indices for the first and last occurrence of each censusType
first_last_positions <- unlist(tapply(1:nrow(data), data$censusType, function(x) c(head(x, 1), tail(x, 1))))

#data <- subset(data, data$censusType != 3) # remove PE from dataset

dataToJags <- list(                                               #liste aggr?g?e d'objets
  n = nrow(data)    
  ,n1=first_last_positions[1:2]
  ,n2=first_last_positions[3:4]
  ,n3=first_last_positions[5:6]
  #N fait n de long avec r?p?titions NA
  #Nriver= length(unique(data$riverID)),
  ,DL1 = data$DL1						
  ,DL2 = data$DL2
  #DL3 = data$DL3
  ,P1 = data$P1
  ,P2 = data$P2 
  ,PE = data$PE # PE
  #n = rep(NA,nrow(data)), 
  ,area = data$area  # we work in log here
  #,is.there.area = data$is.there.area
  ,riverID = data$riverID
  #,vec_riverID=unique(data$riverID),
  #,siteID = data$siteID,  
  ,year= data$year - min(data$year)+1  # it is the year of sampling
  ,popAge=data$popAge  # it is the population age /!\ but by cohort gb+mb 27032024
  #,max_year = max(data$Year_cohort)
  #, year_capture=data$year_capture,
  #,t0= doubtDate+1,
  ,coldate=data$coldate
  ,maxPopAge=maxPopAge
  ,maxMetapopAge=max(data$metapopAge)
  #trueMaxPopAge=trueMaxPopAge
  ,censusType = data$censusType
  )

# De Lury only
data_DL <- subset(data, !is.na(DL1)) # remove PE from dataset
dataToJags_DL <- list(                                               #liste aggr?g?e d'objets
  n = nrow(data_DL)    
  ,DL1 = data_DL$DL1						
  ,DL2 = data_DL$DL2
  #DL3 = data$DL3
  ,area = data_DL$area  # we work in log here
  #,is.there.area = data$is.there.area
  ,riverID = data_DL$riverID
  #,vec_riverID=unique(data$riverID),
  #,siteID = data$siteID,  
  ,year= data_DL$year - min(data_DL$year)+1  # it is the year of sampling
  ,popAge=data_DL$popAge  # it is the population age /!\ but by cohort gb+mb 27032024
  #,max_year = max(data$Year_cohort)
  #, year_capture=data$year_capture,
  #,t0= doubtDate+1,
  #,coldate=data_DL$coldate
  #,maxPopAge=maxPopAge
  ,maxMetapopAge=max(data$metapopAge)
  #trueMaxPopAge=trueMaxPopAge
  #,censusType = data$censusType
)


# Petersen only
data_Petersen <- subset(data, !is.na(P1) & !is.na(P2)) # remove PE from dataset
dataToJags_Petersen <- list(                                               #liste aggr?g?e d'objets
  n = nrow(data_Petersen)    
  ,P1 = data_Petersen$P1						
  ,P2 = data_Petersen$P2
  #DL3 = data$DL3
  ,area = data_Petersen$area  # we work in log here
  #,is.there.area = data$is.there.area
  ,riverID = data_Petersen$riverID
  #,vec_riverID=unique(data$riverID),
  #,siteID = data$siteID,  
  ,year= data_Petersen$year - min(data_Petersen$year)+1  # it is the year of sampling
  ,popAge=data_Petersen$popAge  # it is the population age /!\ but by cohort gb+mb 27032024
  #,max_year = max(data$Year_cohort)
  #, year_capture=data$year_capture,
  #,t0= doubtDate+1,
  #,coldate=data_DL$coldate
  #,maxPopAge=maxPopAge
  ,maxMetapopAge=max(data_Petersen$metapopAge)
  #trueMaxPopAge=trueMaxPopAge
  #,censusType = data$censusType
)

# PE only
data_PE <- subset(data, !is.na(PE)) # remove PE from dataset
dataToJags_PE <- list(                                               #liste aggr?g?e d'objets
  n = nrow(data_PE)    
  ,PE = data_PE$PE						
  #DL3 = data$DL3
  ,area = data_PE$area  # we work in log here
  #,is.there.area = data$is.there.area
  ,riverID = data_PE$riverID
  #,vec_riverID=unique(data$riverID),
  #,siteID = data$siteID,  
  ,year= data_PE$year - min(data_PE$year)+1  # it is the year of sampling
  ,popAge=data_PE$popAge  # it is the population age /!\ but by cohort gb+mb 27032024
  #,max_year = max(data$Year_cohort)
  #, year_capture=data$year_capture,
  #,t0= doubtDate+1,
  #,coldate=data_DL$coldate
  #,maxPopAge=maxPopAge
  ,maxMetapopAge=max(data_PE$metapopAge)
  #trueMaxPopAge=trueMaxPopAge
  #,censusType = data$censusType
)
