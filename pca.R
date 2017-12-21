#On efface les variables en mémoire, permet de faire le ménage avant de commencer
rm(list=ls())
# ouverture des données
#data_tab=read.table("DataEasyNameOnlyStates.csv",header=TRUE,sep=";", dec=",")
data_tab=read.table("DataEasyNameRenorm.csv",header=TRUE,sep=";", dec=",")

######## les donnees chargees viennent de DataEasyNameOnlyStates.csv
# car il faut des noms de colonne sans espace et plus court
# car on enlève la variable globale USA
# car dans cette table, elles ont ete renormalisee
# si on met les vraies valeurs ma PCA est faussee par l'importance d'un revenu annuel face a une pourcentage

# table de donnee de type data.frame
class(data_tab) 

# matrice de données
data=data.matrix(data_tab)
### on enleve les colonnes state et code et population 
#data=data[1:51,3:12]
data=data[1:51,c(3,4,5,6,8,9,10,11,12)]
# on enleve en plus la colonne 7 car salaire median et on a deja le revenu moyen
### on renome les lignes d'après les etats associes
rownames(data)=c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", "FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING")

#### analyse en composantes principales sur la totalité des données

## on verifie que pas de NULL --> OK
any(is.na(data))
## on verifie que c'est des nombres --> OK
any(is.numeric(data))
## on fait la pca
pca = princomp(data)
## on regarde comme elle est repartit
plot(pca)
## quasiment tout est sur la premiere composante et la deuxieme
## on regarde ce que sont les deux premieres composantes
pca$loadings[,1:2]
## portion de variance de chaque composante
100 * pca$sdev^2 / sum(pca$sdev^2)
## variance totale expliquee par les deux premieres composantes
sum(100*(pca$sdev^2)[1:2] / sum(pca$sdev^2))
### 90.17394 % --> pas mal
## biplot sur les deux premieres composantes
plot(pca$scores[,1:2])
biplot(pca)
# et juste sur la premiere
#plot(pca$scores[,1], rep(0,nrow(data)))

####### Meilleure visualisation des resulats de la PCA
#install.packages("factoextra")
library(factoextra)
## scree plot
fviz_eig(pca)

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


