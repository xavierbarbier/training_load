# Création colonne score total
data$total<-data$Sommeil+data$Fatigue+data$Stress+data$Courbatures

ggplot(data, aes(x=Date, y=total,group = 1)) +
  geom_line() + 
  xlab("")

ggplot(data, aes(x=as.Date(Date), y=total,group = 1)) +
  geom_line() + 
  xlab("")  +  theme_classic()

ggplot(data, aes(x=as.factor(Date), y=total)) +
  geom_boxplot() + 
  xlab("")+theme_classic()

ggplot(data, aes(x=Date, y=total, group = 1)) +
  geom_line() + 
  xlab("")+  theme_classic()+facet_wrap(~Athlete)


ggplot(data, aes(x=Date, y=total, group = 1)) +
  geom_bar(stat = 'identity') + 
  xlab("")+  theme_classic()+facet_wrap(~Athlete)

ggplot(data, aes(x=Date, y=total, fill=total)) +
  geom_bar(stat = 'identity') + 
  xlab("")+theme_classic()+facet_wrap(~Athlete)

ggplot(data, aes(x=Date, y=total, fill=total)) +
  geom_bar(stat = 'identity') + 
  xlab("")+
  theme_classic()+facet_wrap(~Athlete)+
  scale_y_continuous(breaks=seq(0,28,14))+
  scale_fill_gradient2(low='blue', mid='green', high='red', midpoint = 14, name='Total')

ggplot(data, aes(x=Athlete, y=total, fill=total)) +
  geom_bar(stat = 'identity') + 
  xlab("")+
  theme_classic()+facet_wrap(~Date)+
  scale_y_continuous(breaks=seq(0,28,14))+
  scale_fill_gradient2(low='blue', mid='green', high='red', midpoint = 14, name='Total')



ggplot(data, aes(x=Date, y=Fatigue, fill=Fatigue)) +
  geom_bar(stat = 'identity') + 
  xlab("")+
  theme_classic()+facet_wrap(~Athlete)+
  scale_y_continuous(breaks=seq(0,7,1))+
  scale_fill_gradient2(low='blue', mid='green', high='red', midpoint = 3.5, name='Fatigue')

install.packages("dplyr")
library(dplyr)


#data frame pour obtenir valeur indiv
athletes_total<-data

#conservation colonnes athletes et total
athletes_total <- select(athletes_total,Athlete,total) 

#Regroupement par athlètes et calcul moyenne
athletes_total<-athletes_total%>%
  group_by(Athlete)%>%
  summarize_all(funs(mean))

#Renommage colonne moyenne
names(athletes_total)[2]<-c("moy_indiv")

athletes_total$moy_indiv<-round(athletes_total$moy_indiv,0)


#graphique total sur période / athlètes
ggplot(data, aes(x=Date, y=total, fill=total)) +
  geom_bar(stat = 'identity') + 
  xlab("")+
  theme_classic()+facet_wrap(~Athlete)+
  scale_y_continuous(breaks=seq(0,28,14))+
  scale_fill_gradient2(low='blue', mid='green', high='red', midpoint = 14, name='Total')

# Graphique binaire (positif ou négatif)
ggplot(data, aes(x=Date, y=total, fill=total)) +
  geom_bar(aes(fill = total > 14),stat = 'identity') + 
  xlab("")+  theme_classic()+facet_wrap(~Athlete)+
  scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("green", "red"))


#graphique total sur période / athlètes selon moyenne équipe

moy_equipe = round(mean(data$total),0)

ggplot(data, aes(x=Date, y=total, fill=total)) +
  geom_bar(aes(fill = total > moy_equipe),stat = 'identity') + 
  xlab("")+  theme_classic()+facet_wrap(~Athlete)+
  scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("green", "red"))

#Diff moyenne équipe et graphique
athletes_total$diff<-athletes_total$moy_indiv-moy_equipe

ggplot(athletes_total, aes(x=Athlete, y=diff, fill=diff)) +
  geom_bar(stat = 'identity') + 
  xlab("")+  theme_classic()

#graphique total sur période / athlètes

##merge data et athlete

data<-merge(data,athletes_total, by="Athlete")

## Graphique comparaison indiv
ggplot(data, aes(x=Date, y=total, fill=total)) +
  geom_bar(aes(fill = total > moy_indiv),stat = 'identity') + 
  xlab("")+  theme_classic()+facet_wrap(~Athlete)+
  scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("green", "red"))

