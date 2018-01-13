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
######################################### scatter plot des donnees

## scatter plot de toutes les donnees 
#plot(data_tab) # trop grand ...

## scatter plot des donnees de regression
palette(c("cornflowerblue","bisque4","firebrick2"))
legend("right", c("D","NP","R"), fill=dataCluster)
#plot(as.data.frame(data), col = dataCluster[,1])
pairs(data, col = dataCluster[,1])
# rq impossible de voir la legende :( ...

# visualisation plus avancee avec ggplot2
library(ggplot2)
library(GGally)
# nuages de points 2 à 2, densité, coeff de corrélation 
#ggpairs(as.data.frame(data), title = "Scatter plot", colour = dataCluster[,1])
ggpairs(data_tab, columns = c(4,5,6,8,11,13,14,15,23), title = "Scatter plot", mapping=ggplot2::aes(colour = Party))

# Function to return points and geom_smooth
# allow for the method to be changed
mysmooth <- function(data, mapping, method="loess", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=method, ...)
  p
}

ggpairs(data_tab, columns = c(4,5,6,8,11,13,14,15,23), lower = list(continuous = wrap(mysmooth, method="lm")), title = "Scatter plot", mapping=aes(colour = Party))

## scatter plot EV et des donnees d'action publique
ggpairs(data_tab, columns = c(4,16,17,18,19,20,21,23), lower = list(continuous = wrap(mysmooth, method="loess")), title = "Scatter plot EV - action publique") #mapping=ggplot2::aes(colour = Party)

### quelques point aberrants
# chercher a quel etat ils correspondent


###################################################################
##################################### zoom sur quelques graphiques
### les evolutions qui semblent les plus pertinentes en zoom

# EV general
plot(data_tab$PEVRegistrations,main="EV Registration",xlab="rank",ylab="PEV Registrations per 1000 People by State")
### identification du point aberrant : la Californie avec 6 EV pour 1000
# par rapport a l'acces aux bornes
plot(data_tab$PEVRegistrations,data_tab$TotalChargingUnits,main="Regression 1",xlab="PEV Registrations per 1000 People by State",ylab="Total Charging Units per 1000 People by State")
plot(data_tab$PEVRegistrations,data_tab$FastChargingUnits,main="Regression 2",xlab="PEV Registrations per 1000 People by State",ylab="Fast Charging Units per 1000 People by State")
## garder les deux car peu de substitution dans les usages
# par rapport au salaire / revenu
plot(data_tab$PEVRegistrations,data_tab$MedianHouseholdIncome,main="Regression 3",xlab="PEV Registrations per 1000 People by State",ylab="Median Household Income")
plot(data_tab$PEVRegistrations,data_tab$RevisedAnnualPersonalIncomePerCapita,main="Regression 4",xlab="PEV Registrations per 1000 People by State",ylab="Revised annual personal income per capita")
## rajouter une variable avec le rapport : median/moyen donne un indice de repartition du salaire
# par rapport au niveau de diplome
plot(data_tab$PEVRegistrations,data_tab$PercentOfHighSchoolDegree,main="Regression 5",xlab="PEV Registrations per 1000 People by State",ylab="Percent Of People 25 Years And Over Who Have Completed High School (Includes Equivalency)")
plot(data_tab$PEVRegistrations,data_tab$PercentOfBachelorDegree,main="Regression 6",xlab="PEV Registrations per 1000 People by State",ylab="Percent Of People 25 Years And Over Who Have Completed A Bachelor's Degree")
plot(data_tab$PEVRegistrations,data_tab$PercentOfAdvancedDegree,main="Regression 7",xlab="PEV Registrations per 1000 People by State",ylab="Percent Of People 25 Years And Over Who Have Completed an Advanced Degree")
# par rapport a agregat de tous les diplomes
## rajouter une variable avec la somme
# par rapport au prix de de l'electricite
plot(data_tab$PEVRegistrations,data_tab$AverageRetailPriceOfElectricity,main="Regression 8",xlab="PEV Registrations per 1000 People by State",ylab="Average retail price of electricity in each State in cents/kWh")
## pour identifier le point bizarre
plot(data_tab$AverageRetailPriceOfElectricity,main="Electricity Price",xlab="Average retail price of electricity in each State in cents/kWh",ylab="rank")
### identification du point aberrant : Hawaii avec 26 cents / kWh
## reste ajouter en fonction de chacun des types de lois ...
