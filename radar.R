#On efface les variables en mémoire, permet de faire le ménage avant de commencer
rm(list=ls())

statenames=c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", "FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING")

###################################################################
########################################### chargement des données

data_tab=read.table("DataEasyNameOnlyStates.csv",header=TRUE,sep=";", dec=",")
class(data_tab) 
data_toAnalyse=data_tab[1:51,c(4,5,6,8,9,11,13,14,15,23)]
maxdata=data_toAnalyse[1,]
mindata=data_toAnalyse[1,]
for (k in 1:10){
  maxdata[k]=max(data_toAnalyse[,k])
  mindata[k]=min(data_toAnalyse[,k])
}
data_toAnalyse = rbind(maxdata, mindata, data_toAnalyse)
rownames(data_toAnalyse)= c("Max", "Min", statenames)
  
###################################################################
############ diagramme de Kiviat (radar chart) pour tous les états

#install.packages("fmsb")
library(fmsb)
COL=colorRampPalette(c("firebrick","cadetblue","chocolate","burlywood","chartreuse","darkmagenta","aquamarine","forestgreen","blueviolet","darkgoldenrod","cornflowerblue","coral","darkseagreen","deeppink","gold","darkred"))(51) 

# pauvres et moins impliqués : Tennessee, Mississippi, South Dakota, South Carolina, Kentucky, Arkansas, Alabama …
# forte action publique et des effets : Californie, Hawaii, Oregon, Washington, Columbia …
# forte action publique sans beaucoup d’effet : Maryland, Idaho, Connecticut, Colorado, Delaware, Texas …
name = "TEXAS"
i = 42
radarchart(data_toAnalyse[c("Max", "Min", name),], axistype = 2, seg = 10, pcol = COL[i], cglcol = "grey80", plty=1, maxmin = TRUE, title = name)

par(mfrow = c(3, 3), mar = c(1,1,1,1))
for (i in 3:11){ 
  radarchart(data_toAnalyse[c(1,2,i),], axistype = 2, seg = 10, pcol = COL[i-2], cglcol = "grey80", plty=1, maxmin = TRUE, title = rownames(data_toAnalyse[i,])) 
}
for (i in 12:20){ 
  radarchart(data_toAnalyse[c(1,2,i),], axistype = 2, seg = 10, pcol = COL[i-2], cglcol = "grey80", plty=1, maxmin = TRUE, title = rownames(data_toAnalyse[i,])) 
}
for (i in 21:29){ 
  radarchart(data_toAnalyse[c(1,2,i),], axistype = 2, seg = 10, pcol = COL[i-2], cglcol = "grey80", plty=1, maxmin = TRUE, title = rownames(data_toAnalyse[i,])) 
}
for (i in 30:38){ 
  radarchart(data_toAnalyse[c(1,2,i),], axistype = 2, seg = 10, pcol = COL[i-2], cglcol = "grey80", plty=1, maxmin = TRUE, title = rownames(data_toAnalyse[i,])) 
}
for (i in 39:47){ 
  radarchart(data_toAnalyse[c(1,2,i),], axistype = 2, seg = 10, pcol = COL[i-2], cglcol = "grey80", plty=1, maxmin = TRUE, title = rownames(data_toAnalyse[i,])) 
}
for (i in 48:52){ 
  radarchart(data_toAnalyse[c(1,2,i),], axistype = 2, seg = 10, pcol = COL[i-2], cglcol = "grey80", plty=1, maxmin = TRUE, title = rownames(data_toAnalyse[i,])) 
}
