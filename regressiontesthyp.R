rm(list=ls())

###################################################################
########################################### chargement des données

# ouverture des données
#lien="C:\\Users\\thibaut\\Documents\\Ponts_2A\\Voiture_Electrique\\telechargement2\\EVanalysis-master\\EVanalysis-master\\DataEasyNameOnlyStates.csv"
#data_tab=read.table(lien,header=TRUE,sep=";", dec=",")
data_tab=read.table("DataEasyNameOnlyStates.csv",header=TRUE,sep=";", dec=",")
class(data_tab) #data frame
# matrice de données
data=data.matrix(data_tab)

dataIncentives=data[1:51,c(16,17,18,19,20,21)]
class(dataIncentives)#matrix
dataIncentives[1:51,1]=dataIncentives[1:51,1]-7500 # on ne garde que le federal
dataCluster=data[1:51,24:27]
data=data[1:51,c(4,5,6,8,11,13,14,15,23)]

rownames(dataIncentives)=c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", "FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING")
rownames(dataCluster)=c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", "FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING")
rownames(data)=c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", "FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING")

###################################################################
##################################### regression lineaire multiple

# the regressions with their results and main statistics

model = lm(PEVRegistrations ~ TotalChargingUnits + FastChargingUnits + MedianHouseholdIncome + PercentOfBachelorDegree + AverageRetailPriceOfElectricity + ResidentialEnergyConsumedPerCapita + RegularGasolinePrice + TotalPublicActionForEV, data=data_tab)
#model = lm(PEVRegistrations ~ Population + TotalChargingUnits + FastChargingUnits + RevisedAnnualPersonalIncomePerCapita + MedianHouseholdIncome + MedianOverMiddleIncome + PercentOfHighSchoolDegree + PercentOfBachelorDegree + PercentOfAdvancedDegree + AverageRetailPriceOfElectricity + ResidentialEnergyConsumedPerCapita + RegularGasolinePrice + IncomeTaxCredit + SalesTax + PurchaseRebate + HOVExemption + ParkingExemption + EmissionInspection + HomeEVSE + TotalPublicActionForEV + Party + Green + Green2 + GreenGB , data=data_tab)

summary(model) 
anova(model)
coefficients(model)

summary.aov(model)

layout(matrix(1:4,2,2))
plot(model)


#http://162.38.181.25/LinuxHelp/wp-content/uploads/2011/05/GuideTP.pdf

model.res<- residuals(model)
hist(model.res)
shapiro.test(model.res)

par(mfrow =c(2, 2),oma=c(0, 0, 2, 0))
plot(model)

sink(file="A.txt") 

sink()
#q()

#install.packages("car")
#install.packages("lmtest")
library(lmtest)
hmctest(model)
# HMC = 0.7096, p-value = 0.991
dwtest(model)
# DW = 2.0315, p-value = 0.5438

