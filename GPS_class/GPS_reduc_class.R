install.packages("dplyr")
install.packages("GGally")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("reshape2")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("FactoInvestigate")
install.packages("Hmisc")
install.packages("gridExtra")

library(gridExtra)
library(Hmisc)
library(FactoMineR)
library(factoextra)
library(FactoInvestigate)
library(reshape2)
library(lubridate)
library(ggplot2)
library(GGally)
library(dplyr)

# # Import des données

library(readr)
data <- read_csv("GPS/data/Workload Data.csv", 
                          col_types = cols(Date = col_date(format = "%m/%d/%Y")))
View(data)

# recherche valeurs nulles
sum(is.na(data)) #0

# renommage colonnes MPM -> m.min

names(data)[8]<-"M.min"

# renommage colonnes Sprint Distance -> Sprint

names(data)[10]<-"SpD"

# renommage colonnes max

names(data)[7]<-"max_speed"

# renommage colonnes

names(data)[4]<-"session_type"
names(data)[11]<-"session_rpe"

data$week<-week(data$Date)
data$day<-weekdays(data$Date)

# suppression colonnes inutilisées
data<-data[,-1]
data<- data[,-10]

## Classique -----------------------------------------------------------

# Conservation données initiales
classique <- data

# Analyse descriptive

grid.table(summary(classique[4:9]))

# Matrice par paires
ggpairs(classique[c(4:9)],upper = "blank",aes(colour = as.factor(classique$session_type),
                                               alpha = 0.4), legends = TRUE)+
  theme(legend.position = "bottom")

# Matrice corrélations
ggcorr(classique[c(4:9)], palette = "RdBu", label = TRUE)

# changement de format
classique_melt <- melt(classique,id=c("Date","Name","week","day", "session_type"))

# graphique rapport
ggplot(classique_melt[classique_melt$Name == "Athlete 01" & classique_melt$week == 1,],
       aes(x = Date,y= value, fill=variable))+geom_bar(stat = 'identity')+
  facet_wrap(~variable,scales = "free")+ theme_classic()+
  ggtitle("Atlete 01 - Semaine 1")

# sauvegarde données en csv
write.csv(classique,"gps_classique.csv", row.names = FALSE)



# Multimecanical load ----------------------------------------------------------

# calcul HSR.min

data$HSD.min <- round(data$HSD/data$Time,0)

# calcul Sp.min

data$SpD.min <- round(data$SpD/data$Time,0)


# création df match

match <- data%>%filter(session_type == "Match")

# Création max TD

max_td <- match%>% group_by(match$Name)%>% summarise(max(TD))

names(max_td)[1]<-"Name"
names(max_td)[2]<-"TD.max"

# Création max HSD
max_hsd <- match%>% group_by(match$Name)%>% summarise(max(HSD))

names(max_hsd)[1]<-"Name"
names(max_hsd)[2]<-"HSD.max"

# Création max Sprint distance
max_sprint <- match%>% group_by(match$Name)%>% summarise(max(SpD))

names(max_sprint)[1]<-"Name"
names(max_sprint)[2]<-"SpD.max"

# Merge différent max sur data

data<-merge(data,max_hsd, by = "Name")
data<-merge(data,max_td, by = "Name")
data<-merge(data,max_sprint, by = "Name")

# Création max TD.min
max_td_min <- match%>% group_by(match$Name)%>% summarise(max(M.min))

names(max_td_min)[1]<-"Name"
names(max_td_min)[2]<-"M.min.max"

# Création max HSR.min
max_hsd_min <- match%>% group_by(match$Name)%>% summarise(max(HSD.min))

names(max_hsd_min)[1]<-"Name"
names(max_hsd_min)[2]<-"HSD.min.max"

# Création max Sp.min

max_sprint_min <- match%>% group_by(match$Name)%>% summarise(max(SpD.min))

names(max_sprint_min)[1]<-"Name"
names(max_sprint_min)[2]<-"SpD.min.max"

# Merge différent max.min sur data

data<-merge(data,max_hsd_min, by = "Name")
data<-merge(data,max_td_min, by = "Name")
data<-merge(data,max_sprint_min, by = "Name")

# calcul volume

data$Volume <- round((((data$TD/data$TD.max)+(data$HSD/data$HSD.max)+(data$SpD/data$SpD.max))/3)*100,0)

# calcul intensité
data$Intensite <- round((((data$M.min/data$M.min.max)+(data$HSD.min/data$HSD.min.max)+(data$SpD.min/data$SpD.min.max))/3)*100,0)

# conservation data complètes
multi <- data[,c(1:3,10:11,20:21)]

# Descriptif
grid.table(summary(multi[,6:7]))

# Matrice par paires
ggpairs(multi[,6:7],upper = "blank",aes(colour = as.factor(classique$session_type),
                                        alpha = 0.4))+ 
  theme(legend.position = "bottom")

# Matrice corrélations
ggcorr(multi[,6:7], palette = "RdBu", label = TRUE)

# changement de format
multi_melt <- melt(multi,id=c("Date","Name","week","day", "session_type"))

# graphique rapport
ggplot(multi_melt[multi_melt$Name == "Athlete 01" & multi_melt$week == 1,],
       aes(x = Date,y= value, fill=variable))+geom_bar(stat = 'identity')+
  facet_wrap(~variable,scales = "free")+ theme_classic()+
  ggtitle("Atlete 01 - Semaine 1")

# sauvegardes données en format csv
write.csv(classique,"gps_classique.csv", row.names = FALSE)



# ACP ----------------------------

# acp
res <- PCA(data[,c(4:9,12:13)], graph = FALSE)

# Eboulis valeurs propres
eig.val <- res$eig

barplot(eig.val[, 2], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances Explained by PCs (%)",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
lines(x = 1:nrow(eig.val), eig.val[, 2], 
      type = "b", pch = 19, col = "red")

## cercle des corrélations 1er plan
fviz_pca_var(res, col.var = "cos2",
             gradient.cols = c("white", "blue", "red"),
             repel = TRUE 
)

# Cos2 total des variables sur Dim.1 et Dim.2
fviz_cos2(res, choice = "var", axes = 1:2)



# Coordonées plan 1
coord<-as.data.frame(res$ind$coord)
coord<-coord[,1:2]

# regroupement data et dimensions
data<-cbind(data,coord)

# conversion dimension en pourcentage
data$Dim.1<-round(pnorm(data$Dim.1)*100,0)
data$Dim.2<-round(pnorm(data$Dim.2)*100,0)

# changement de nom dimensions acp
names(data)[22]<-"int_acp"
names(data)[23]<-"vol_acp"

# Descriptif
grid.table(summary(data[,c(22:23)]))

# Matrice corrélations
ggcorr(data[,c(22:23)], palette = "RdBu", label = TRUE)

# Matrice par paires
ggpairs(data[,c(22:23)],upper = "blank",aes(colour = as.factor(data$session_type),
                                           alpha = 0.4))+ 
  theme(legend.position = "bottom")

# conservation données complètes
data_acp <- data[,c(1:3,10:11,22:23)]

# changement de format
data_acp_melt<-  melt(data_acp,id=c("Date","Name","week","day", "session_type"))

# graphique rapport
ggplot(data_acp_melt[data_acp_melt$Name == "Athlete 01" & data_acp_melt$week == 1,],
       aes(x = Date,y= value, fill=value))+geom_bar(stat = 'identity')+
  facet_wrap(~variable,scales = "free")+ theme_classic()+
  ggtitle("Atlete 01 - Semaine 1")



## cluster------------------------------------------------------------

# HCPC
res.hcpc <- HCPC(res, graph = FALSE)

# affichage des clusters
fviz_cluster(res.hcpc,
             geom = "point",            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)

# Récupération cluster
cluster<-as.data.frame(res.hcpc[["data.clust"]][["clust"]])
names(cluster)[1]<-"cluster"

# regroupement data et clusters
data<-cbind(data,cluster)

# assignation cluster 4 aux matchs
data$cluster<-ifelse(data$session_type == "Match",4,data$cluster)


# projection des  individus selon cluster
fviz_pca_ind(res,
             geom.ind = "point", # Montre les points seulement (mais pas le "text")
             col.ind = as.factor(data$cluster), # colorer by groups
             addEllipses = TRUE, # Ellipses de concentration
             legend.title = "Groups"
)



# conservation données complètes
data_cluster_classique <- data[,c(1:9,24)]

# changement de format  
data_cluster_classique_melt<-  melt(data_cluster_classique,id=c("Date","Name",
                                                 "cluster", "session_type"))
# graphique variables selon cluster
ggplot(data_cluster_classique_melt,aes(x = as.factor(cluster),
                      y = value, fill = as.factor(cluster)))+geom_boxplot()+
  facet_wrap(~variable,scales = "free")+ theme_classic()

# conservation données complètes
data_cluster_multi <- data[,c(1:3,20:21,24)]

# changement de format 
data_cluster_multi_melt<-  melt(data_cluster_multi,id=c("Date","Name",
                                                      "cluster", "session_type"))
# graphique variables selon cluster
ggplot(data_cluster_multi_melt,aes(x = as.factor(cluster),
                                       y = value, fill = as.factor(cluster)))+geom_boxplot()+
  facet_wrap(~variable,scales = "free")+ theme_classic()

# conservation données complètes
data_cluster_acp <- data[,c(1:3,22:24)]

# changement de format 
data_cluster_acp_melt<-  melt(data_cluster_acp,id=c("Date","Name",
                                                        "cluster", "session_type"))
# graphique variables selon cluster
ggplot(data_cluster_acp_melt,aes(x = as.factor(cluster),
                                   y = value, fill = as.factor(cluster)))+geom_boxplot()+
  facet_wrap(~variable,scales = "free")+ theme_classic()


# barplot session cluster

ggplot(data, aes(x = cluster, fill = session_type))+geom_bar(position="fill")+
  ggtitle("Composition des clusters")+ theme_classic() 

