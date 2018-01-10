#On efface les variables en m?moire, permet de faire le m?nage avant de commencer
rm(list=ls())
# ouverture des donn?es
data_tab=read.table("DataEasyNameOnlyStates.csv",header=TRUE,sep=";", dec=",")
#data_tab=read.table("C:\\Users\\thibaut\\Documents\\Ponts_2A\\Voiture_Electrique\\EVanalysis-master\\EVanalysis-master\\DataEasyNameOnlyStates.csv",header=TRUE,sep=";", dec=",")

# table de donnee de type data.frame
class(data_tab) 


attach(data_tab)
ls()

resume <- function(variable) {
print('mean=')
print(mean(variable))

print('median=')
print(median(variable))

print('standard deviation=')
print(sd(variable))

plot(x = Code,y=variable)

}

names_list=colnames(data_tab)

resume(Population)
resume(PEVRegistrations)
resume(TotalChargingUnits)
resume(FastChargingUnits)
resume(RevisedAnnualPersonalIncomePerCapita)


########################## compléments : fonctions automatiques de R
data=data.matrix(data_tab)
dataIncentives=data[1:51,c(16,17,18,19,20,21)]
data=data[1:51,c(4,5,6,8,11,13,14,15,23)]
dataIncentives[1:51,1]=dataIncentives[1:51,1]-7500 # on ne garde que le federal

rownames(data)=c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", "FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING")
rownames(dataIncentives)=c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", "FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING")

summary(data)
summary(dataIncentives)

######################### histogramme des variables

library(ggplot2)
# Histogramme basique avec courbe de densité
ggplot(data_tab, aes(x=PEVRegistrations)) + geom_histogram(binwidth=0.2) + geom_density(alpha=.2, fill="#FF6666") 
ggplot(data_tab, aes(x=TotalChargingUnits)) + geom_histogram(binwidth=0.02) + geom_density(alpha=.2, fill="#FF6666") 
ggplot(data_tab, aes(x=FastChargingUnits)) + geom_histogram(binwidth=0.004) + geom_density(alpha=.2, fill="#FF6666") 
ggplot(data_tab, aes(x=MedianHouseholdIncome)) + geom_histogram(binwidth=500) + geom_density(alpha=.2, fill="#FF6666") 
ggplot(data_tab, aes(x=PercentOfBachelorDegree)) + geom_histogram(binwidth=0.01) + geom_density(alpha=.2, fill="#FF6666") 
ggplot(data_tab, aes(x=AverageRetailPriceOfElectricity)) + geom_histogram(binwidth=0.5) + geom_density(alpha=.2, fill="#FF6666") 
ggplot(data_tab, aes(x=ResidentialEnergyConsumedPerCapita)) + geom_histogram(binwidth=10) + geom_density(alpha=.2, fill="#FF6666") 
ggplot(data_tab, aes(x=RegularGasolinePrice)) + geom_histogram(binwidth=0.02) + geom_density(alpha=.2, fill="#FF6666") 
ggplot(data_tab, aes(x=TotalPublicActionForEV)) + geom_histogram(binwidth=200) + geom_density(alpha=.2, fill="#FF6666") 

### beacoup mieux interprétable
#### rajouter des courbes de densité et histogrammes différenciés (couleurs) selon le parti 
ggplot(data_tab, aes(x=PEVRegistrations, color=Party, fill=Party)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+ geom_density(alpha=.2) 
ggplot(data_tab, aes(x=TotalChargingUnits, color=Party, fill=Party)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+ geom_density(alpha=.2) 
ggplot(data_tab, aes(x=FastChargingUnits, color=Party, fill=Party)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+ geom_density(alpha=.2) 
ggplot(data_tab, aes(x=MedianHouseholdIncome, color=Party, fill=Party)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+ geom_density(alpha=.2) 
ggplot(data_tab, aes(x=PercentOfBachelorDegree, color=Party, fill=Party)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+ geom_density(alpha=.2) 
ggplot(data_tab, aes(x=AverageRetailPriceOfElectricity, color=Party, fill=Party)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+ geom_density(alpha=.2) 
ggplot(data_tab, aes(x=ResidentialEnergyConsumedPerCapita, color=Party, fill=Party)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+ geom_density(alpha=.2) 
ggplot(data_tab, aes(x=RegularGasolinePrice, color=Party, fill=Party)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+ geom_density(alpha=.2) 
ggplot(data_tab, aes(x=TotalPublicActionForEV, color=Party, fill=Party)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+ geom_density(alpha=.2) 

#### rajouter des courbes de densité et histogrammes différenciés (couleurs) selon nos 2 cluster kmeans
