# Caractérisation de séances d'entrainement à partir des données GPS

## Présentation
GPS largement utilisés dans le suivi de l'entrainement dans les sports collectifs pour suivre la préparation à la compétition.

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

## Partie 1 : réduction dimensions
### Variables classiques
Volume :
* TD (total distance)

Intensité : 
* max_speed
* M.min

Volume/Intensité
* HSD (high speed distance)
* SpD (sprint distance)

#### Résumé
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/describ_classique.png)

-> jeu de données clairement fictif avec une vitesse max médiane à 8km/h.

#### Matrice par paires
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/matrice_paires_classique.png)

#### Matrice corrélations
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/corrplot_classique.png)

#### Rapport
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/athlete1_sem1_classique.png)

### Multimodal
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

#### Résumé
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/describ_multi.png)

#### Matrice par paires
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/matrice_paires_multi.png)

#### Matrice corrélations
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/corrplot_multi.png)

#### Rapport
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/athlete1_sem1_multi.png)


### ACP
Variables utiliées : 
* Time
* TD (total distance)
* max_speed
* M.min
* HSD (high speed distance)
* SpD (sprint distance)
* HSD.min 
* SpD.min 

#### Eboulis des valeurs propres
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/evp.png)

#### Qualité repésentation variables
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/representation_p1.png)

#### Cercle de corrélations
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/cercle_correlations.png)

* Dim.1 probablement = Intensité
* Dim.2 probablement = Volume

#### Résumé
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/describ_acp.png)

#### Matrice par paires
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/matrice_paires_acp.png)

#### Matrice corrélations
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/corrplot_acp.png)

#### Rapport
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/athlete1_sem1_acp.png)

## Partie 2 : classification des séances

### Demandes selon classification actuelle
#### Classique 
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/session_type_classique.png)

#### Multimodal 
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/session_type_multi.png)

#### ACP 
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/session_type_acp.png)

* Pas de différences visibles selon thème de séance.

### Hierarchical Clustering on Principle Components (HCPC)

#### Clusters
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/cluster_p1.png)
* 3 clusters

#### Clusters avec Match
Match = cluster 4
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/cluster_session.png)

### Demandes selon classification HCPC
#### Classique 
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/cluster_classique.png)

#### Multimodal 
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/cluster_multi.png)

#### ACP 
![100% center](https://github.com/xavierbarbier/training_load/blob/master/GPS_class/graph/cluster_acp.png)


## Références
* *EXTRAIT 01 SDS_#24 L'utilisation Des Techno GPS - Romain Dubois - https://fr.calameo.com/read/003232740fb4f2a88b009
* *A contemporary multi-modal mechanical approach to training monitoring in elite professional soccer - Owen & col. - https://www.researchgate.net/publication/318379665_A_contemporary_multi-modal_mechanical_approach_to_training_monitoring_in_elite_professional_soccer
* *Dubois R, Bru N, Paillard T, Le Cunuder A, Lyons M, Maurelli O, et al. (2020) Rugby game performances and weekly workload: Using of data mining process to enter in the complexity. PLoS ONE 15(1): e0228107. https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0228107



