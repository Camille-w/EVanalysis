#On efface les variables en mémoire, permet de faire le ménage avant de commencer
rm(list=ls())

###################################################################
########################################### chargement des données

# ouverture des données
data_tab=read.table("DataEasyNameOnlyStates.csv",header=TRUE,sep=";", dec=",")
class(data_tab) 
# matrice de données
data=data.matrix(data_tab)
#data=data[1:51,c(4,5,6,8,9,11,13,14,15,16,17,18,20,21,22)]
#data=data[1:51,c(4,5,6,8,9,11,13,14,15,23)]
#dataIncentives=data[1:51,c(16,17,18,19,20,21,22)]
dataIncentives=data[1:51,c(16,17,18,19,20,21)]
dataIncentives[1:51,1]=dataIncentives[1:51,1]-7500 # on ne garde que le federal
dataCluster=data[1:51,24:27]
data=data[1:51,c(4,5,6,8,11,13,14,15,23)]

rownames(dataIncentives)=c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", "FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING")
rownames(dataCluster)=c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", "FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING")
rownames(data)=c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", "FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING")

###################################################################
############## analyse en composantes principales sur la selection

any(is.na(data))
any(is.numeric(data))

## pca normalisee avec affichage
library("FactoMineR")
pca=PCA(data, scale.unit = TRUE, ncp = 5, graph = TRUE)

library("corrplot")
Var_contribution = corrplot(pca$var$contrib, is.corr=FALSE)
Var_representation = corrplot(pca$var$cos2, is.corr=FALSE)

library(factoextra)
screeplot = fviz_eig(pca)
fviz_cos2(pca, choice = "var", axes = 1:2)
fviz_contrib(pca, choice = "var", axes = 1:2, top = 10)
PCAindividus = fviz_pca_ind(pca,col.ind = "cos2",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
PCAvar = fviz_pca_var(pca,col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
PCAbiplot = fviz_pca_biplot(pca, repel = TRUE,col.var = "#2E9FDF",col.ind = "#696969")

###################################################################
################## PCA sur les mesures d'action publiques par type

any(is.na(dataIncentives))
any(is.numeric(dataIncentives))

pcaIncentives=PCA(dataIncentives, scale.unit = TRUE, ncp = 5, graph = TRUE)

fviz_eig(pcaIncentives)
fviz_pca_ind(pcaIncentives,col.ind = "cos2",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
fviz_pca_var(pcaIncentives,col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
fviz_pca_biplot(pcaIncentives, repel = TRUE,col.var = "#2E9FDF",col.ind = "#696969")


###################################################################
################### clustering et coloration de la pca par groupes


### evaluating the goodness of clustering
get_clust_tendency(scale(data),n = 50, gradient = list(low = "steelblue",  high = "white"))
# 0.25... = quite close to 0 = the dataset is significantly clusterable

## representation de la matrice des distances
viewdist=get_dist(data, stand = TRUE, method = "pearson")
fviz_dist(viewdist,gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#nombre de groupes desirés
# pour choisir l'optimal
fviz_nbclust(scale(data), kmeans, method = "gap_stat")+labs(subtitle = "Gap statistic method")
bestCluster1 = fviz_nbclust(scale(data), kmeans, method = "silhouette")+labs(subtitle = "Silhouette method")
fviz_nbclust(scale(data), kmeans, method = "wss")+labs(subtitle = "Elbow method")
### conseil 2 cluster

## avec un rapport d'analyse
library("NbClust")
mynbclust=NbClust(scale(data),distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all") 
bestCluster2 = fviz_nbclust(mynbclust, ggtheme = theme_minimal())
### conseil 3 cluster

# le desire 
nbgr=2

########### partionning clustering

## formation des groupes avec kmeans
mygr=kmeans(scale(data), centers = nbgr, nstart = 25)
grp=as.factor(mygr$cluster)

## ou avec la library cluster
library("cluster")
mypam=pam(scale(data),nbgr)
fviz_cluster(mypam)

#### visualisations possibles
fviz_cluster(mygr, data=data, palette = "jco",ggtheme = theme_minimal())
fviz_pca_ind(pca,col.ind=grp,palette = "jco",addEllipses = FALSE,legend.title = "Cluster")
fviz_pca_biplot (pca, col.ind = grp, palette = "jco",addEllipses = TRUE, label = "var",col.var = "black", repel = TRUE,legend.title = "Cluster")
cluster_biplot = fviz_pca_biplot(pca, 
                # Individus
                #geom.ind = "point",
                fill.ind = grp, col.ind = "black",
                pointshape = 21, pointsize = "cos2",palette = "jco",
                addEllipses = TRUE,
                repel = TRUE,
                # Variables
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = list(fill = "Cluster kmeans", color = "Contrib")
)

cluster_ind = fviz_pca_ind(pca, fill.ind = grp, col.ind = "black",pointshape = 21, 
                              pointsize = "cos2",palette = "jco",addEllipses = TRUE,repel = TRUE,
)
print(cluster_ind)

#### hierarchical clustering : creating a dendogram

# we scale the data and then compute the dissimilarity matrix and the hierachical clustering
myhc =hclust(dist(scale(data),method = "euclidean"),method = "ward.D2") 
# Visualize : Cut in nbgr groups and color by groups
dendogram = fviz_dend(myhc, k = nbgr, cex = 0.5, k_colors = "jco", color_labels_by_k = TRUE, rect = TRUE)

# Inspect the silhouette plot
myhc=eclust(scale(data),"hclust", k = nbgr, graph = FALSE)
fviz_silhouette(myhc)
fviz_dend(myhc, palette = "jco",rect = TRUE, show_labels = TRUE)

###################################################################
################## comparaison avec les cluster politique et ecolo

party=as.factor(dataCluster[1:51,1])
fviz_pca_ind(pca,col.ind=party,palette = c("blue","purple","red"),addEllipses = FALSE,repel = TRUE, legend.title = "Cluster")
fviz_pca_biplot(pca, repel = TRUE,col.var = "black",col.ind = party, palette = c("blue","purple","red"))

fviz_pca_ind(pca, fill.ind = grp, col.ind = party,pointshape = 21,pointsize = "cos2",palette = "jco",addEllipses = TRUE,repel = TRUE)
#fviz_dend(myhc, k = nbgr, cex = 0.5, k_colors = "Dark2", label_cols=party, rect = TRUE)

ecolo=as.factor(dataCluster[1:51,4])
fviz_pca_ind(pca,col.ind=ecolo,palette = c("grey","green","blue"),addEllipses = FALSE,repel = TRUE, legend.title = "Cluster")
fviz_pca_biplot(pca, repel = TRUE,col.var = "black",col.ind = ecolo, palette = c("grey","green","blue"))

fviz_pca_ind(pca, fill.ind = grp, col.ind = ecolo,pointshape = 21,pointsize = "cos2",palette = "jco",addEllipses = TRUE,repel = TRUE)
#fviz_dend(myhc, k = nbgr, cex = 0.5, k_colors = "Dark2", label_cols=ecolo, rect = TRUE)

###################################################################
#################### clustering sur les mesures d'action publiques

get_clust_tendency(scale(dataIncentives),n = 50, gradient = list(low = "steelblue",  high = "white"))
# 0.20... = quite close to 0 = the dataset is significantly clusterable

fviz_nbclust(scale(dataIncentives), kmeans, method = "gap_stat")+labs(subtitle = "Gap statistic method")
fviz_nbclust(scale(dataIncentives), kmeans, method = "silhouette")+labs(subtitle = "Silhouette method")
mynbclustIncentives=NbClust(scale(dataIncentives),distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all") 
fviz_nbclust(mynbclustIncentives, ggtheme = theme_minimal())
### conseil 4 cluster (ou 7 ou 2 ou 10)
nbgrIncentives=4

#### partionning clustering
mygrIncentives=kmeans(scale(dataIncentives), centers = nbgrIncentives, nstart = 25)
grpIncentives=as.factor(mygrIncentives$cluster)
fviz_pca_biplot(pcaIncentives,fill.ind = grpIncentives, col.ind = "black", pointshape = 21, 
                pointsize = "cos2",palette = "jco",addEllipses = TRUE,repel = TRUE,
                col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = list(fill = "Cluster kmeans", color = "Contrib")
)

#### hierarchical clustering 

myhcIncentives =hclust(dist(scale(dataIncentives),method = "euclidean"),method = "ward.D2") 
fviz_dend(myhcIncentives, k = nbgrIncentives, cex = 0.5, k_colors = "jco", color_labels_by_k = TRUE, rect = TRUE)


###################################################################
############# en vue de l'enregistrement automatique des resultats

# a completer avec les noms des graphes une fois ceux a enregistrer choisis

# dans un nouveau  pdf
#pdf ("PCA.pdf") 
#print (myplot)
#dev.off ()

# dans plusieurs nouveau png
library(ggpubr)
#ggexport (plotlist = list(myplot1, myplot2, myplot3),filename = "PCA.png")
ggexport (plotlist = list(Var_contribution, Var_representation, PCAbiplot,PCAindividus,PCAvar),filename = "PCA.png")
ggexport (plotlist = list(bestCluster1, bestCluster2, cluster_biplot, cluster_ind,dendogram),filename = "Cluster.png")

