library(ggplot2)
library(dplyr)
library(hrbrthemes)

#setwd("C:/Users/gmbrahy/Desktop/these/Density")
data1<-read.table("data/datajakdenyoung.txt",h=T)
data1$age <- 1
data2<-read.table("data/datajakdenadult.txt",h=T)
data2$age <- 2
data<-rbind(data1,data2)
data$area <- gsub(",", ".", data$area)
data$area <- as.numeric(data$area)
data$year<-as.numeric(substr(data$date, 7, 10))
data$month<-as.numeric(substr(data$date, 4, 5)) 


str(data[,c(5:8,10:13)])

##Matrice spatio-temporelle données

#Calculer colonnes abondance, DL, P
data <- data %>%
  mutate(abundance = rowSums(across(c(DL1, DL2, DL3, DL4, P1, P2, P3, PE)), na.rm = TRUE),
         DL = rowSums(across(c(DL1, DL2, DL3, DL4)), na.rm = TRUE),
         P  = rowSums(across(c(P1, P2, P3)), na.rm=TRUE)
         )
###Ajouter colonne méthode de pêche 
#Vérifier qu'une ligne ne contient pas deux méthodes de pêche
# any(!is.na(data$PE) & data$PE !=0 & !is.na(data$Petersen) & data$Petersen !=0) #vérifier pour 0 et pour NA
# any(!is.na(data$PE) & data$PE !=0 & !is.na(data$Delury) & data$Delury !=0)#11 lignes pechees en PE et delury
# any(!is.na(data$Petersen) & data$Petersen !=0 & !is.na(data$Delury) & data$Delury !=0)PE_et_delury <- data %>% 
#   filter(!is.na(data$PE) & data$PE !=0 & !is.na(data$Delury) & data$Delury !=0)

##Créer nouvelle colonne qui contient la méthode de pêche (si PE et Delury -> Delury)

data <- data %>%
  mutate(
    Method = ifelse(DL != 0, "Delury",
                    ifelse(P != 0, "Petersen",
                           ifelse(!is.na(PE) & PE != 0, "PE", NA)))
  )

##Visualisation des données

colours <- c("Delury"="pink", "Petersen"="lightblue", "PE"="lightgreen")

ggplot(data[data$basin == "Studer", ], aes(x=as.factor(year), y=abundance, color=Method)) + 
  geom_jitter(position=position_jitter(width=0.3)) + 
  labs(x = "Year", y = "Abundance") + 
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8))+
  theme(legend.position = c(0.92, 1),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))+
  ggtitle(paste("Abondance par année et méthode, rivière:Studer"))+
  scale_colour_manual(values = colours)

Studer <- subset(data, data$basin=="Studer")
Norvegienne <- subset(data, data$basin=="Norvegienne")
# ggplot(Studer, aes(x=as.factor(year), y=abundance, colour = Method)) + 
#   geom_bar(stat="identity", position=position_dodge(width=0.7))+
#   theme_minimal()

ggplot(Studer, aes(x=as.factor(year), y=abundance, color=Method)) + 
  geom_jitter(position=position_jitter(width=0.3)) + 
  labs(x = "Year", y = "Abundance", title = "Abundance by Year and Method") + 
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8))

ggplot(Studer, aes(x=as.factor(month), y=abundance)) + 
  geom_bar(stat="identity", fill="lightblue") + 
  labs(x = "Month", y = "Abundance", title = "Abundance by Month") + 
  theme_minimal()

ggplot(Norvegienne, aes(x=as.factor(year), y=area)) + 
  geom_boxplot(fill="lightgreen") + 
  labs(x = "Year", y = "Area (m^2)", title = "Area by year") + 
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8))

barplot(height=Studer$area, names=Studer$year,main="Aire par année", xlab="Année d'échantillonnage", ylab="Area (m^2)", col="blue")


#Visualisation des données - boucle sur bassins
library(gridExtra)
basins <- levels(as.factor(data$basin))
pdf(file="results/Data_exploration.pdf")

#Abondance par méthode de pêche

ggplot(data, aes(x = Method, y = abundance)) + 
  geom_bar(stat="identity", fill="#ADD8E6", alpha=0.7) +
  theme_minimal() +
  labs(title = "Abundance per sampling method", 
       x = "Method", 
       y = "Abundance")


#Abondances par bassin et année
ggplot(data, aes(x = year, y = basin, color = abundance)) + 
  geom_point(size=3) +
  scale_color_gradient(low = "#ADD8E6", high = "#00008B") +
  theme_minimal() +
  labs(title = "Abundance Heatmap ", 
       x = "Année", 
       y = "Bassin")

ggplot(data, aes(x = year, y = basin, shape = Method, color = abundance)) + 
  geom_point(size=3) +
  scale_color_gradient(low = "#ADD8E6", high = "#00008B") +
  theme_minimal() +
  labs(title = "Abundance and method Heatmap ", 
       x = "Année", 
       y = "Bassin",
       shape = "Méthode")

#Distribution des aires par méthode de pêche
ggplot(data, aes(x = Method, y = basin, color = area)) + 
  geom_point(size=3) +
  scale_color_gradient(low = "#ADD8E6", high = "#00008B") +
  theme_minimal() +
  labs(title = "Heatmap des aires par bassin et méthode de pêche", 
       x = "Method", 
       y = "Basin")

colours <- c("Delury"="#EE6677", "Petersen"="#56B4E9", "PE"="lightgreen")

ggplot(data, aes(x=Method, y=area, colour = Method))+
  geom_boxplot()+
  scale_colour_manual(values = colours)+
  ggtitle("Area sampled vs sampling method")


#Distribution des aires par bassin
ggplot(data, aes(x = area, y = basin)) + 
  geom_point(size=3) +
  theme_minimal() +
  labs(title = "Area distribution per basin", 
       x = "area", 
       y = "Basin")


ggplot(data, aes(x = year, y = area, color = Method)) + 
  geom_point() +
  theme_minimal() +
  scale_colour_manual(values = colours)+
  labs(title = "Area in time and per method", 
       x = "Year", 
       y = "Area")

# Créer une boucle pour chaque niveau de basin
for (basin in basins) {
  # Filtrer les données pour le niveau actuel de basin
  subset_data <- data[data$basin == basin, ]
  
  # Configurer l'agencement des graphiques 
  #par(mfrow=c(3,1))
  
  # Créer le premier graphique ggplot pour le niveau actuel de basin
 p1 <- ggplot(subset_data, aes(x=as.factor(month), y=abundance)) + 
   geom_bar(stat="identity", fill="lightblue") + 
   labs(x = "Month", y = "Abundance") + 
   ggtitle(paste("Abondance par mois, rivière:", basin))+
   theme_minimal()
 #print(p1)
 

 #abondance par année et selon méthode (superposer les histo)
 p2 <-  ggplot(subset_data, aes(x=as.factor(year), y=abundance, color=Method)) + 
   geom_jitter(position=position_jitter(width=0.3)) + 
   labs(x = "Year", y = "Abundance") + 
   theme_minimal()+
   theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8),
         legend.position = c(0.92, 1),
         legend.title = element_text(size = 10),
         legend.text = element_text(size = 8))+
   ggtitle(paste("Abondance par année et méthode, rivière:", basin))+
   scale_colour_manual(values = colours)
 #print(p2)
 
 p3 <-   ggplot(subset_data, aes(x=as.factor(year), y=area, colour = Method)) + 
   geom_boxplot() + 
   labs(x = "Year", y = "Area (m^2)") + 
   ggtitle(paste("Aire par année, rivière:", basin))+
   theme_minimal()+
   theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8),
         legend.position = c(0.92, 1),
         legend.title = element_text(size = 10),
         legend.text = element_text(size = 8))+
   scale_colour_manual(values = colours)
   #print(p3)

 #aires par année
 #barplot(height=as.numeric(subset_data$area), names=as.factor(subset_data$year),main=paste("Aire par année",basin), xlab="Année d'échantillonnage", ylab="Aire (m^2)", col="blue")#ylim = 3000)
 #barplot(height=as.numeric(subset_data$abundance), names=as.factor(subset_data$month),main=paste("Abondance par mois", basin), xlab="Mois", ylab="Abondance", col="blue")#,ylim=60000)
 grid.arrange(p1, p2, p3, nrow=3)
}
dev.off()


