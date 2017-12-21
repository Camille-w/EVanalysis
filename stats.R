#On efface les variables en mémoire, permet de faire le ménage avant de commencer
rm(list=ls())
# ouverture des données
data_tab=read.table("DataEasyName.csv",header=TRUE,sep=";")

# table de donnee de type data.frame
class(data_tab) 

# matrice de données
data=as.matrix(data_tab)
