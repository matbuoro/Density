library(ggplot2)
library(dplyr)
library(hrbrthemes)

#setwd("C:/Users/gmbrahy/Desktop/these/Density")
# data1<-read.table("data/datajakdenyoung.txt",h=T)
# data1$age <- 1
# data2<-read.table("data/datajakdenadult.txt",h=T)
# data2$age <- 2
# data<-rbind(data1,data2)
data<-read.table("data/dataoctobre.txt",h=T, fill=TRUE)
#data$area <- gsub(",", ".", data$area)
#data$area <- as.numeric(data$area)
data <- subset(data, data$basin != "Chateau_>_Ferme_ou_Etangs")
data$estArea <- as.numeric(data$estArea)
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










##Visualisation des données - boucle sur bassins

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


colours <- c("Delury"="#EE6677", "Petersen"="#56B4E9", "PE"="lightgreen")

a1 <- ggplot(data, aes(x=Method, y=area, colour = Method))+
  geom_boxplot()+
  scale_colour_manual(values = colours)+
  ggtitle("Area vs sampling method")

a2 <- ggplot(data, aes(x=Method, y=estArea, colour = Method))+
  geom_boxplot()+
  scale_colour_manual(values = colours)+
  ggtitle(" Estimated Area vs sampling method")

grid.arrange(a1, a2, nrow = 2)


#Distribution des aires par bassin
a3 <- ggplot(data, aes(x = area, y = basin, color=Method)) + 
  geom_point(size=2) +
  theme_minimal() +
  labs(title = "Area distribution per basin", 
       x = "Area", 
       y = "Basin")+
  theme(
    plot.title = element_text(size = 12, hjust = 0),
    plot.title.position = "plot",
    axis.text.x = element_text(size = 8),  
    axis.text.y = element_text(size = 8),
    legend.text = element_text(size = 7)
    )

a4 <- ggplot(data, aes(x = estArea, y = basin, color=Method)) + 
  geom_point(size=2, show.legend = FALSE) +
  theme_minimal() +
  labs(title = "Estimated area distribution per basin", 
       x = "Estimated area", 
       y = "Basin")+
  theme(
    plot.title = element_text(size = 12, hjust = 0),
    plot.title.position = "plot",
    axis.text.x = element_text(size = 8),  
    axis.text.y = element_text(size = 8)   
  )

grid.arrange(a3, a4, ncol = 2)

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


 p3 <-   ggplot(subset_data, aes(x=as.factor(year), y=area, colour = Method)) +
   geom_jitter(position=position_jitter(width=0.3))+
   labs(x = "Year", y = "Area (m^2)") +
   ggtitle(paste("Aire par année, rivière:", basin))+
   theme_minimal()+
   theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8),
         legend.position = c(0.92, 1),
         legend.title = element_text(size = 10),
         legend.text = element_text(size = 8))+
   scale_colour_manual(values = colours)
   #print(p3)
 
 p4 <- ggplot(subset_data, aes(x=as.factor(year), y=estArea, color = Method)) +
  geom_jitter(position = position_jitter(width = 0.3))+
    labs(x = "Year", y= "Estimated Area (m^2")+
    ggtitle(paste("Aire estimée par année, rivière:", basin))+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          legend.position = c(0.92,1),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8))+
    scale_color_manual(values = colours)
 
 # #pour le graphe 3 - pour visualiser séparément area et estArea - ça ne marche pas dans la boucle 
 # data_long <- data.frame(
 #   year = rep(subset_data$year, 2),
 #   Value = c(subset_data$area, Studer$estArea),
 #   Method = rep(subset_data$Method, 2),
 #   Type = rep(c("area", "estArea"), each = nrow(subset_data))
 # )
 # p3 <- ggplot(data_long, aes(x = as.factor(year), y = Value, colour = Method, shape = Type)) + 
 #   geom_jitter(position = position_jitter(width = 0.3), size = 2) +  # Ajuster la taille ici
 #   labs(x = "Year", y = "Area (m²)", shape = "Type of Area") + 
 #   ggtitle(paste("Aire par année, rivière:", basin)) +
 #   theme_minimal() +
 #   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
 #         legend.position = c(0.92, 1),
 #         legend.title = element_text(size = 10),
 #         legend.text = element_text(size = 8)) +
 #   scale_colour_manual(values = colours) +
 #   scale_shape_manual(values = c(16, 15), labels = c("Area", "Estimated Area"))  # 16 pour ronds, 15 pour carrés

 grid.arrange(p1, p2, p3, p4, nrow = 4)
}
dev.off()


