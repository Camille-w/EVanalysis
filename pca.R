#On efface les variables en mémoire, permet de faire le ménage avant de commencer
rm(list=ls())

###################################################################
########################################### chargement des données

# ouverture des données
data_tab=read.table("DataEasyNameOnlyStates.csv",header=TRUE,sep=";", dec=",")

######## les donnees chargees viennent de DataEasyNameOnlyStates.csv
# car il faut des noms de colonne sans espace et plus court
# car on enlève la variable globale USA
# dans la suite les valeurs seront renormalisées (pour etre comparables) dans les fonctions

# table de donnee de type data.frame
class(data_tab) 

# matrice de données
data=data.matrix(data_tab)
#data=data[1:51,3:12]
#data=data[1:51,c(4,5,6,8,9,10,11,12,13)]
data=data[1:51,c(4,5,6,8,9,11,13)]
# on enleve les colonnes 1,2,3 : info gen inutiles pour pca 
# et 7 pour garder un seul revenu
# et 10,12 pour garder un seul niveau de diplome
### on renome les lignes d'après les etats associes
rownames(data)=c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", "FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING")


###################################################################
############## analyse en composantes principales sur la selection

## on verifie que pas de NULL --> OK
any(is.na(data))
## on verifie que c'est des nombres --> OK
any(is.numeric(data))

## on fait la pca
library("FactoMineR")
# pca avec normalisation (scale=TRUE)
pca=PCA(data, scale.unit = TRUE, ncp = 5, graph = TRUE)
## et on regarde comme elle est repartie (graph=TRUE)
#install.packages("corrplot")
library("corrplot")

## on regarde ce que sont les composantes
pca$eig
corrplot(pca$var$contrib, is.corr=FALSE)
## portion de variance de chaque composante (pour les n°1 et 2)
pca$eig[1:2,2]
## et variance totale expliquee par les deux premieres composantes
pca$eig[1:2,3]
### que 67 % 
# estimation de la qualité de reprsenation pour chaque type de données
pca$var$cos2
corrplot(pca$var$cos2, is.corr=FALSE)

## biplot sur les deux premieres composantes
plot(pca)

####### Meilleure visualisation des resulats de la PCA
#install.packages("factoextra")
library(factoextra)
## scree plot
fviz_eig(pca)

# Cos2 total des variables sur Dim1 et Dim2
fviz_cos2(pca, choice = "var", axes = 1:2)

#contribution des colonnes de données
fviz_contrib(pca, choice = "var", axes = 1:2, top = 10)

## Graphique des individus avec coloration selon la qualité de représentation
# Les individus similaires sont groupés ensemble par couleur
fviz_pca_ind(pca,col.ind = "cos2",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
## Utah et Maine tres peu explique (<5%)
# Alaska et Rhode Isalnd bof aussi 

## Graphique des variables avec coloration en fonction de la contribution des variables. 
# Les variables corrélées positivement sont du même côté du graphique. 
# Les variables corrélées négativement sont sur des côtés opposés du graphique.
fviz_pca_var(pca,col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)

## Biplot des individus et des variables
fviz_pca_biplot(pca, repel = TRUE,col.var = "#2E9FDF",col.ind = "#696969")

###################################################################
############# en vue de l'enregistrement automatique des resultats

# a completer avec les noms des graphes une fois ceux a enregistrer choisis

# dans un nouveau  pdf
#pdf ("PCA.pdf") 
#print (myplot)
#dev.off ()

# dans plusieurs nouveau png
#ggexport (plotlist = list(myplot1, myplot2, myplot3),filename = "PCA.png")
