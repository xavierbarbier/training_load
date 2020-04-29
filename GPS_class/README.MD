# Caractérisation de séances d'entrainement à partir des données GPS

## Présentation
GPS largement utilisés dans le suivi de l'entrainement dans les sports collectifs

PARTIE 1
* Très nombreuses variables à dispositions des staffs
* Problématique de communication des données et prise de décision à partir de celles-ci
* Plusieurs méthodes de présentation des données utilisés par les staffs
* Besoin de simplification

-> réduction de dimmension pour meilleure communication

PARTIE 2 
* Augmentation d'une approche intégrée de la préparation
* Difficulté de qualification des séances

-> classification des séances selon demandes physique

## Code et ressources
* Jeu de données fourni pas Adam Sullivan https://adam-sullivan.com/free-downloads/
* Jeu de données fictif
* Analyse réalisé sur RStudio Cloud

## Nettoyage des données
* Aucune données manquantes

## Préparation des données
Renommage ou création de plusieurs variables. 
Variables finales : 
* Name
* Date
* Session_type
* Time
* TD (total distance)
* max_speed
* M.min
* HSD (high speed distance)
* SpD (sprint distance)
* week
* day

## Variables classiques
Volume :
* TD (total distance)

Intensité : 
* max_speed
* M.min

Volume/Intensité
* HSD (high speed distance)
* SpD (sprint distance)

### Résumé
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/describ_classique.png)

-> jeu de données clairement fictif avec une vitesse max médiane à 8km/h.

### Matrice par paires
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/matrice_paires_classique.png)

### Matrice corrélations
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/corrplot_classique.png)

### Rapport
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/athlete1_sem1_classique.png)

## Modele multivarié
* Multi-modal mechanical
* ratio séance/max match

Volume :
* TD (total distance)
* HSD (high speed distance)
* SpD (sprint distance)

Intensité : 
* M.min
* HSD.min 
* SpD.min 

-> données A:D manquantes.

### Résumé
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/describ_multi.png)

### Matrice par paires
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/matrice_paires_multi.png)

### Matrice corrélations
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/corrplot_multi.png)

### Rapport
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/athlete1_sem1_multi.png)


## ACP
