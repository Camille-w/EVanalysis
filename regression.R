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
##################################### regression lineaire multiple

# the regressions with their results and main statistics

## function of all variables
#regall = lm(data[1:51,1] ~ data[1:51,2] + data[1:51,3] + data[1:51,4] + data[1:51,5] + data[1:51,6] + data[1:51,7] + data[1:51,8] + data[1:51,9])
regall = lm(PEVRegistrations ~ TotalChargingUnits + FastChargingUnits + MedianHouseholdIncome + PercentOfBachelorDegree + AverageRetailPriceOfElectricity + ResidentialEnergyConsumedPerCapita + RegularGasolinePrice + TotalPublicActionForEV, data=data_tab)
summary(regall) 

## function of all variables
regallType = lm(PEVRegistrations ~ TotalChargingUnits + FastChargingUnits + MedianHouseholdIncome + PercentOfBachelorDegree + AverageRetailPriceOfElectricity + ResidentialEnergyConsumedPerCapita + RegularGasolinePrice 
                + IncomeTaxCredit + SalesTax + PurchaseRebate + HOVExemption + ParkingExemption + EmissionInspection + HomeEVSE, data=data_tab)
summary(regallType) 

## function of certain variables
## on enlève FastChargingUnits et AverageRetailPriceOfElectricity, ResidentialEnergyConsumedPerCapita
reg1 = lm(PEVRegistrations ~ TotalChargingUnits + TotalPublicActionForEV + MedianHouseholdIncome + PercentOfBachelorDegree + RegularGasolinePrice, data=data_tab)
summary(reg1)

## function de l'action publique
regInc = lm(PEVRegistrations ~ TotalPublicActionForEV, data=data_tab)
summary(regInc)

## function de l'action publique par mesure
regIncType = lm(PEVRegistrations ~ IncomeTaxCredit + SalesTax + PurchaseRebate + HOVExemption + ParkingExemption + EmissionInspection + HomeEVSE, data=data_tab)
summary(regIncType) 

### comparaison de modeles
anova(regInc,regIncType)

# les coefficients du modele
#coefficients(reg1)
# Intervalles de consifance des paramètre du modele 
#confint(reg1, level=0.95) 
# valeurs predites
#fitted(reg1) 
# residus
#residuals(reg1)
# table d'analyse de variance
#anova(reg1)  
# matrice de covariance pour les parametres du modele
#vcov(reg1)  
# diagnostics de regression 
#influence(reg1) 

# diagnostic plots (les 4 graphes sur la meme fenetre) 
layout(matrix(c(1,2,3,4),2,2))  
plot(reg1)
# provide checks for heteroscedasticity, normality, and influential observerations.



# Stepwise model selection by exact AIC in the Regression
library(MASS)
step=stepAIC(regallType, direction="both")
step$anova # display results

step=stepAIC(regall, direction="both")
step$anova



# All Subsets Regression
library(leaps)
attach(mydata)
leaps=regsubsets(PEVRegistrations ~ TotalChargingUnits + FastChargingUnits + MedianHouseholdIncome + PercentOfBachelorDegree + AverageRetailPriceOfElectricity + ResidentialEnergyConsumedPerCapita + RegularGasolinePrice 
                 + IncomeTaxCredit + SalesTax + PurchaseRebate + HOVExemption + ParkingExemption + EmissionInspection + HomeEVSE, 
                 data=data_tab,nbest=10)
# view results 
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
#plot(leaps,scale="r2")
# plot statistic by subset size 
library(car)
#subsets(leaps, statistic="rsq")


# Calculate Relative Importance for Each Predictor
library(relaimpo)
calc.relimp(regallType,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples) 
boot <- boot.relimp(regallType, b = 1000, type = c("lmg","last", "first", "pratt"), 
                    rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result


###################################################################
################################# regression log-lineaire multiple

lndata_tab=data_tab
lndata_tab$PEVRegistrations=log(lndata_tab$PEVRegistrations)
lndata_tab$TotalChargingUnits=log(lndata_tab$TotalChargingUnits)
lndata_tab$FastChargingUnits=log(lndata_tab$FastChargingUnits)
lndata_tab$MedianHouseholdIncome=log(lndata_tab$MedianHouseholdIncome)
lndata_tab$PercentOfBachelorDegree=log(lndata_tab$PercentOfBachelorDegree)
lndata_tab$AverageRetailPriceOfElectricity=log(lndata_tab$AverageRetailPriceOfElectricity)
lndata_tab$ResidentialEnergyConsumedPerCapita=log(lndata_tab$ResidentialEnergyConsumedPerCapita)
lndata_tab$RegularGasolinePrice=log(lndata_tab$RegularGasolinePrice)
lndata_tab$IncomeTaxCredit=log(lndata_tab$IncomeTaxCredit)
lndata_tab$SalesTax=log(lndata_tab$SalesTax)
lndata_tab$PurchaseRebate=log(lndata_tab$PurchaseRebate)
lndata_tab$HOVExemption=log(lndata_tab$HOVExemption)
lndata_tab$ParkingExemption=log(lndata_tab$ParkingExemption)
lndata_tab$EmissionInspection=log(lndata_tab$EmissionInspection)
lndata_tab$HomeEVSE=log(lndata_tab$HomeEVSE)
lndata_tab$TotalPublicActionForEV=log(lndata_tab$TotalPublicActionForEV)

# the regressions with their results and main statistics

## function of all variables
#lnregallType = lm(PEVRegistrations ~ TotalChargingUnits + FastChargingUnits + MedianHouseholdIncome 
                #+ PercentOfBachelorDegree + AverageRetailPriceOfElectricity + ResidentialEnergyConsumedPerCapita + RegularGasolinePrice 
                #+ TotalPublicActionForEV, data=lndata_tab)
                ####+ IncomeTaxCredit + SalesTax + PurchaseRebate + HOVExemption + ParkingExemption + EmissionInspection + HomeEVSE, data=lndata_tab)
#summary(lnregallType) 

