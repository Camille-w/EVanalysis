#On efface les variables en mémoire, permet de faire le ménage avant de commencer
rm(list=ls())
# ouverture des données
#data_tab=read.table("DataEasyNameOnlyStates.csv",header=TRUE,sep=";")
data_tab=read.table("DataEasyNameOnlyStates.csv",header=TRUE,sep=";", dec=",")

# table de donnee de type data.frame
class(data_tab) 

# matrice de données
data=as.matrix(data_tab)
#m=matrix(runif(100),10,10)
### on enleve les colonnes state et code et population ?
data=data[1:51,4:13]

## scatter plot des donnees 
plot(data_tab)
### quelques point aberrants
# chercher a quel etat ils correspondent

### les evolutions qui semblent pertinentes 
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
## reste ajouter en fonction de chacun des types de mesures

#### analyse en composantes principales sur la totalité des données
#res_pca = princomp(data)
### probleme : pourquoi ???
## on verifie que pas de NULL
any(is.na(data))
## on verifie que tout est reconnu comme des nombres

#print(names(res_pca))


