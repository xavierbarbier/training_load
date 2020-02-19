# Importation données depuis le tableur
url<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vS9rrpWQjqDa9DtQBdiHI-HTXhIp5sESN9wmDsctFs6Rso5exaYmRWCOvKXgSXMAtOUul7R3syfr071/pub?gid=1908985686&single=true&output=csv"

#Lecture des données csv
data <- read.csv(url)

# lecture 6 premières lignes
head(data)
View(data)

# Renommage des colonnes
names(data)[1] <- "Date"
names(data)[2] <- "Athlete"
names(data)[3] <- "Sommeil"
names(data)[4] <- "Fatigue"
names(data)[5] <- "Stress"
names(data)[6] <- "Courbatures"

#Chiffres clés
summary(data)

# Histogramme des différentes variables
par(mfcol = c(2, 2))
hist(data$Sommeil, main = "Sommeil", xlab="Note 1-7")
hist(data$Fatigue, main = "Fatigue",xlab="Note 1-7")
hist(data$Stress, main = "Stress",xlab="Note 1-7")
hist(data$Courbatures, main = "Courbatures",xlab="Note 1-7")