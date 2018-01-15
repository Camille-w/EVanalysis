#On efface les variables en mémoire, permet de faire le ménage avant de commencer
rm(list=ls())

###################################################################
########################################### chargement des données

# ouverture des données
data_tab=read.table("DataEasyNameOnlyStates.csv",header=TRUE,sep=";", dec=",")
class(data_tab) 
attach(data_tab)
# matrice de données
data=data.matrix(data_tab)
#data=data[1:51,c(4,5,6,8,9,11,13,14,15,16,17,18,20,21,22)]
#data=data[1:51,c(4,5,6,8,9,11,13,14,15,23)]
#dataIncentives=data[1:51,c(16,17,18,19,20,21,22)]
dataIncentives=data[1:51,c(16,17,18,19,20,21)]
dataIncentives[1:51,1]=dataIncentives[1:51,1]-7500 # on ne garde que le federal
dataCluster=data[1:51,24:27]
data=data[1:51,c(4,5,6,8,11,13,14,15,23)]
# on regroupe les actions publiques selon 2 groupe : effet lors de l'achat ou de l'usage
dataIncentivesGrouped=dataIncentives[1:51,c(1,4)]
dataIncentivesGrouped[1:51,1]=dataIncentives[1:51,1]+dataIncentives[1:51,2]+dataIncentives[1:51,3] 
dataIncentivesGrouped[1:51,2]=dataIncentives[1:51,4]+dataIncentives[1:51,5]+dataIncentives[1:51,6]
colnames(dataIncentivesGrouped)=c("mesure à l'achat","mesure à l'usage")

rownames(dataIncentives)=c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", "FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING")
rownames(dataCluster)=c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", "FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING")
rownames(data)=c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", "FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING")

###################################################################
##################################### rappel des clusters obtenus

### depuis les données
# cluster politique
party=as.factor(dataCluster[1:51,1])
# cluster ecolo
ecolo=as.factor(dataCluster[1:51,4])
### avec KMeans (cf pcaCluster.R)
# cluster de tendance global sur les donnees de data
clusterGlobal=as.factor(kmeans(scale(data), centers = 2, nstart = 25)$cluster)
# cluster de tendance global sur les donnees d'action publique
clusterIncentives=as.factor(kmeans(scale(dataIncentives), centers = 4, nstart = 25)$cluster)
# calcul d'un cluster de tendance global sur les donnees d'action publique regroupees en 2
# sur ces donnees regroupees, nombre optimal de 3 cluster 
clusterIncentivesGrouped=as.factor(kmeans(scale(dataIncentivesGrouped), centers = 3, nstart = 25)$cluster)
# 1 = beacoup achat et assez peu usage ; 2 = très peu achat et peu usage ; 3 = beaucoup usage
# plot(dataIncentivesGrouped, col = clusterIncentivesGrouped)

###################################################################
##################################### regression lineaire multiple
# the regressions with their results and main statistics

#### pour un model note reg1 on peut acceder a :
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

##########################################################
######## Actions Publiques toutes regroupees

## function que de l'action publique
regAP = lm(PEVRegistrations ~ TotalPublicActionForEV, data=data_tab)
summary(regAP)

## toutes les autres variables comprises, tout etat confondu
#regall = lm(data[1:51,1] ~ data[1:51,2] + data[1:51,3] + data[1:51,4] + data[1:51,5] + data[1:51,6] + data[1:51,7] + data[1:51,8] + data[1:51,9])
regall = lm(PEVRegistrations ~ TotalChargingUnits + FastChargingUnits + MedianHouseholdIncome + PercentOfBachelorDegree + AverageRetailPriceOfElectricity + ResidentialEnergyConsumedPerCapita + RegularGasolinePrice + TotalPublicActionForEV, data=data_tab)
summary(regall) 

## mise a jour manuelle en enlevant les elements peu significatifs
regall2 = regall
regall2 = update(regall2,  ~ .-FastChargingUnits)
summary(regall2)
regall2 = update(regall2,  ~ .-MedianHouseholdIncome)
summary(regall2)
regall2 = update(regall2,  ~ .-AverageRetailPriceOfElectricity)
summary(regall2)
regall2 = update(regall2,  ~ .-PercentOfBachelorDegree)
summary(regall2)
regall2 = update(regall2,  ~ .-ResidentialEnergyConsumedPerCapita)
summary(regall2)

# selection du meilleur modele inclut dans regall par coeffs de Mallow
library(leaps)
mallow = leaps(x=data[,2:9],y=data[,1], 
               names=c("TotalChargingUnits","FastChargingUnits","MedianHouseholdIncome","PercentOfBachelorDegree","AverageRetailPriceOfElectricity",
                       "ResidentialEnergyConsumedPerCapita","RegularGasolinePrice","TotalPublicActionForEV"),
               method="Cp",nbest=5)
plot(mallow$size,mallow$Cp)
min(mallow$Cp) 
# 6.477762 : le n°21
mallow$which[21,]
# indique d'enlever MedianHouseholdIncome AverageRetailPriceOfElectricity FastChargingUnits
regall3 = regall
regall3 = update(regall3,  ~ .-FastChargingUnits-MedianHouseholdIncome-AverageRetailPriceOfElectricity)
summary(regall3)

# on compare le modele epure choisi a l'original
anova(regall,regall3)

## separement selon les cluster d'action publique
# les cluster sont trop petits pour lm : des NA apparaissent 
# les donnees extraites et les regressions associees 
#VeryLittle1 = data_tab[ which(clusterIncentives==2), ]
#PurchaseRebateandMix = data_tab[ which(clusterIncentives==3), ]
#HOVandIcomeTax = data_tab[ which(clusterIncentives==4), ]
#SalesTax = data_tab[ which(clusterIncentives==1), ]
#VeryLittle = data_tab[ which(clusterIncentivesGrouped==1), ]
#WithUse = data_tab[ which(clusterIncentivesGrouped==2), ]
#AtPurchase = data_tab[ which(clusterIncentivesGrouped==3), ]
# les regressions ne fonctionnent pas sur la plupart de ces sous-groupes
# donnent beacoup de NA car peu de points donc plus de points tres aberrants (singularities)
#summary(lm(PEVRegistrations ~ TotalChargingUnits + FastChargingUnits + MedianHouseholdIncome + PercentOfBachelorDegree + AverageRetailPriceOfElectricity + ResidentialEnergyConsumedPerCapita + RegularGasolinePrice + TotalPublicActionForEV, data=data_tab[ which(clusterIncentivesGrouped==2 | clusterIncentivesGrouped==3), ]))
### pas de groupes de cluster sur l'action publique ayant une signification et fonctionnant avec lm pour ce model

#########################
############ test de la stabilite structurelle : Chow
#install.packages("strucchange")
library(strucchange)
sctest(PEVRegistrations ~ TotalChargingUnits + PercentOfBachelorDegree + 
         + ResidentialEnergyConsumedPerCapita + RegularGasolinePrice + 
         + TotalPublicActionForEV, type="Chow", data = data_tab)
# pas de structural break
# possible de remplacer type="Chow" par type="Rec-CUSUM" ou par type="Nyblom-Hansen"
#library(gap)
#chow.test(data_tab[ which(clusterIncentives==2), 4],data_tab[ which(clusterIncentives==2), c(5,11,14,15,23)],data_tab[ which(clusterIncentives==1 | clusterIncentives==3 | clusterIncentives==4), 4],data_tab[ which(clusterIncentives==1 | clusterIncentives==3 | clusterIncentives==4), c(5,11,14,15,23)])
# not working ...
### sur le type de politique publique
m1=lm(PEVRegistrations ~ TotalChargingUnits + PercentOfBachelorDegree + 
        ResidentialEnergyConsumedPerCapita + RegularGasolinePrice + 
        TotalPublicActionForEV, data = data_tab[ which(clusterIncentives==2), ])
m2=lm(PEVRegistrations ~ TotalChargingUnits + PercentOfBachelorDegree + 
        ResidentialEnergyConsumedPerCapita + RegularGasolinePrice + TotalPublicActionForEV, 
      data = data_tab[ which(clusterIncentives==1 | clusterIncentives==3 | clusterIncentives==4), ])
an0 = anova(regall3)
an1 = anova(m1)
an2 = anova(m2)
fcrit=qf(.95,df1=m1$df,df2=m2$df)
Chow_Statistic=((an0[6,2]-(an1[6,2]+an2[6,2]))/6)/((an1[6,2]+an2[6,2])/(m1$df+m2$df-(2*6)))
print(Chow_Statistic)
print(fcrit)
# pas de structural break
### sur le parti politique
m1=lm(PEVRegistrations ~ TotalChargingUnits + PercentOfBachelorDegree + 
        ResidentialEnergyConsumedPerCapita + RegularGasolinePrice + 
        TotalPublicActionForEV, data = data_tab[ which(party==1), ])
m2=lm(PEVRegistrations ~ TotalChargingUnits + PercentOfBachelorDegree + 
        ResidentialEnergyConsumedPerCapita + RegularGasolinePrice + TotalPublicActionForEV, 
      data = data_tab[ which(party==2 | party==3), ])
an0 = anova(regall3)
an1 = anova(m1)
an2 = anova(m2)
fcrit=qf(.95,df1=m1$df,df2=m2$df)
Chow_Statistic=((an0[6,2]-(an1[6,2]+an2[6,2]))/6)/((an1[6,2]+an2[6,2])/(m1$df+m2$df-(2*6)))
print(Chow_Statistic)
print(fcrit)
# structural break !!!

#layout(matrix(c(1,2,3,4),2,2))  
#plot(regall)
par(mfrow =c(2, 3),oma=c(2, 2, 2, 2))
# diagnostic plots (les 4 graphes sur la meme fenetre) 
plot(regall3, sub.caption = "Diagnostic plots pour le modele (1*)")
# provide checks for heteroscedasticity, normality, and influential observerations.
# histogramme des résidus
hist(resid(regall3), breaks = 15, main = "Histograms of Residuals", xlab = "Residuals", ylab = "Density")
boxplot(residuals(regall3), main = "Boxplot of Residuals")

#########################
############ Heteroscedasticity test
# Breush Pagan Test
library(lmtest)
bptest(regall3)
# BP = 13.838, df = 5, p-value = 0.01667
library(car)
ncvTest(regall3)
# Chisquare = 53.22594    Df = 1     p = 2.973063e-13
# White Test
# install.packages("het.test")
library(het.test)
whites.htest(VAR(data.frame(x=TotalChargingUnits + PercentOfBachelorDegree + ResidentialEnergyConsumedPerCapita 
                            + RegularGasolinePrice + TotalPublicActionForEV, y=PEVRegistrations), p=1))
# Harrison-McCabe test
hmctest(regall3)
# Shapiro test sur la normalité des residus
shapiro.test(residuals(regall3))

#########################
############ Heteroscedasticity Correction
# Box-Cox transformation 
library(caret)
data_tabBC = data_tab
data_tabBC$PEVRegistrations = predict(BoxCoxTrans(data_tab$PEVRegistrations), data_tab$PEVRegistrations)
#data_tabBC$TotalChargingUnits = predict(BoxCoxTrans(data_tab$TotalChargingUnits), data_tab$TotalChargingUnits)

regallBC = lm(PEVRegistrations ~ TotalChargingUnits + PercentOfBachelorDegree +
                ResidentialEnergyConsumedPerCapita + RegularGasolinePrice + 
                TotalPublicActionForEV, data = data_tabBC)
summary(regallBC)
bptest(regallBC)
# BP = 1.0926, df = 5, p-value = 0.9548
# 0.9548 beaucoup mieux que 0.01667 MAIS ... peut peut-être mieux
plot(regallBC, sub.caption = "Diagnostic plots pour le modele (1*) avec Box-Cox transformation")
hist(resid(regallBC), breaks = 15, main = "Histograms of Residuals", xlab = "Residuals", ylab = "Density")
boxplot(residuals(regallBC), main = "Boxplot of Residuals")
shapiro.test(residuals(regallBC))
# residus accepter comme normaux

#### Correction by Feasible Generalized Least Squared (FGLS)
# comparaison entre nos residus et les White robust standard errors
library(sandwich)
coeftest(regall3, vcov = vcovHC(regall3, "HC1"))
resi = regall3$residuals
regVarfunc = lm(log(resi^2) ~ log(TotalChargingUnits) + log(PercentOfBachelorDegree) +
                  log(ResidentialEnergyConsumedPerCapita) + log(RegularGasolinePrice) + 
                  log(TotalPublicActionForEV), data = data_tab)
varfunc=exp(regVarfunc$fitted.values)
regallFGLS = lm(PEVRegistrations ~ TotalChargingUnits + PercentOfBachelorDegree +
                ResidentialEnergyConsumedPerCapita + RegularGasolinePrice + 
                TotalPublicActionForEV, weights = 1/sqrt(varfunc), data = data_tab)
summary(regallFGLS)
bptest(regallFGLS)
# BP = 1.0926, df = 5, p-value = 0.9548
# 0.9548 beaucoup mieux que 0.01667 MAIS ... peut peut-être mieux
plot(regallFGLS, sub.caption = "Diagnostic plots pour le modele (1*) avec FGLS Correction")
hist(resid(regallFGLS), breaks = 15, main = "Histograms of Residuals", xlab = "Residuals", ylab = "Density")
boxplot(residuals(regallFGLS), main = "Boxplot of Residuals")

#########################
############ Autocorrelation test 
## Durbin Watson
dwtest(regall3)
# DW = 2.0101, p-value = 0.5216
durbinWatsonTest(regall3)
# lag Autocorrelation D-W Statistic p-value
#  1     -0.01123168      2.010111   0.976
### doute ... mais "true autocorrelation is greater than 0"


##########################################################
######## Actions Publiques toutes separees

## function que de l'action publique par mesure
regIncType = lm(PEVRegistrations ~ IncomeTaxCredit + SalesTax + PurchaseRebate + HOVExemption + ParkingExemption + EmissionInspection + HomeEVSE, data=data_tab)
summary(regIncType) 

## function of all variables
regallbyType = lm(PEVRegistrations ~ TotalChargingUnits + FastChargingUnits + MedianHouseholdIncome + PercentOfBachelorDegree + AverageRetailPriceOfElectricity + ResidentialEnergyConsumedPerCapita + RegularGasolinePrice 
                + IncomeTaxCredit + SalesTax + PurchaseRebate + HOVExemption + ParkingExemption + EmissionInspection, data=data_tab)
summary(regallbyType) 

# selection du meilleur modele inclut dans regall par coeffs de Mallow
mallowbyType = leaps(x=data_tab[,c(5,6,8,11,13,14,15,16,17,18,19,20,21)],y=data_tab[,4], 
               names=colnames(data_tab)[c(5,6,8,11,13,14,15,16,17,18,19,20,21)], method="Cp",nbest=5)
plot(mallowbyType$size,mallowbyType$Cp)
min(mallowbyType$Cp) 
# 5.609269 : le n°26
mallowbyType$which[26,]
# on enleve ce qui est indique : FastChargingUnits, MedianHouseholdIncome, ...
regallbyType2 = regallbyType
regallbyType2 = update(regallbyType2,  ~ .-FastChargingUnits-MedianHouseholdIncome-PercentOfBachelorDegree-ResidentialEnergyConsumedPerCapita-IncomeTaxCredit-SalesTax-EmissionInspection)
summary(regallbyType2)

# on compare le modele epure choisi a l'original
anova(regallbyType,regallbyType2)

#### test de la stabilite structurelle : Chow
sctest(PEVRegistrations ~ TotalChargingUnits + AverageRetailPriceOfElectricity + 
         RegularGasolinePrice + PurchaseRebate + HOVExemption + ParkingExemption, 
       type="Chow", data = data_tab)
# pas de structural break
### sur le type de politique publique
m1=lm(PEVRegistrations ~ TotalChargingUnits + AverageRetailPriceOfElectricity + 
        RegularGasolinePrice + PurchaseRebate + HOVExemption + ParkingExemption, 
      data = data_tab[ which(clusterIncentives==2), ])
m2=lm(PEVRegistrations ~ TotalChargingUnits + AverageRetailPriceOfElectricity + 
        RegularGasolinePrice + PurchaseRebate + HOVExemption + ParkingExemption, 
      data = data_tab[ which(clusterIncentives==1 | clusterIncentives==3 | clusterIncentives==4), ])
an0 = anova(regall3)
an1 = anova(m1)
an2 = anova(m2)
fcrit=qf(.95,df1=m1$df,df2=m2$df)
Chow_Statistic=((an0[6,2]-(an1[6,2]+an2[6,2]))/6)/((an1[6,2]+an2[6,2])/(m1$df+m2$df-(2*6)))
print(Chow_Statistic)
print(fcrit)
# pas de structural break
### sur le parti politique
m1=lm(PEVRegistrations ~ TotalChargingUnits + AverageRetailPriceOfElectricity + 
        RegularGasolinePrice + PurchaseRebate + HOVExemption + ParkingExemption, 
      data = data_tab[ which(party==1), ])
m2=lm(PEVRegistrations ~ TotalChargingUnits + AverageRetailPriceOfElectricity + 
        RegularGasolinePrice + PurchaseRebate + HOVExemption + ParkingExemption, 
      data = data_tab[ which(party==2 | party==3), ])
an0 = anova(regall3)
an1 = anova(m1)
an2 = anova(m2)
fcrit=qf(.95,df1=m1$df,df2=m2$df)
Chow_Statistic=((an0[6,2]-(an1[6,2]+an2[6,2]))/6)/((an1[6,2]+an2[6,2])/(m1$df+m2$df-(2*6)))
print(Chow_Statistic)
print(fcrit)
# pas de structural break

# diagnostic plots (les 4 graphes sur la meme fenetre) 
layout(matrix(c(1,2,3,4),2,2))  
plot(regallbyType)

par(mfrow =c(2, 3),oma=c(2, 2, 2, 2))
# diagnostic plots (les 4 graphes sur la meme fenetre) 
plot(regallbyType2, sub.caption = "Diagnostic plots pour le modele (2*)")
# provide checks for heteroscedasticity, normality, and influential observerations.
# histogramme des résidus
hist(resid(regallbyType2), breaks = 15, main = "Histograms of Residuals", xlab = "Residuals", ylab = "Density")
boxplot(residuals(regallbyType2), main = "Boxplot of Residuals")

#########################
############ Heteroscedasticity test
# Breush Pagan Test
#library(lmtest)
bptest(regallbyType2)
# BP = 18.971, df = 6, p-value = 0.004214
#library(car)
ncvTest(regallbyType2)
# Chisquare = 50.21008    Df = 1     p = 1.381364e-12
# White Test
#library(het.test)
whites.htest(VAR(data.frame(x=TotalChargingUnits + AverageRetailPriceOfElectricity + RegularGasolinePrice 
                            + PurchaseRebate + HOVExemption + ParkingExemption, y=PEVRegistrations), p=1))
# Harrison-McCabe test
hmctest(regallbyType2)
# HMC = 0.69368, p-value = 0.984

#########################
############ Heteroscedasticity Correction
# Box-Cox transformation
#library(caret)
data_tabBC = data_tab
data_tabBC$PEVRegistrations = predict(BoxCoxTrans(data_tab$PEVRegistrations), data_tab$PEVRegistrations)
data_tabBC$TotalChargingUnits = predict(BoxCoxTrans(data_tab$TotalChargingUnits), data_tab$TotalChargingUnits)

regallbyTypeBC = lm(PEVRegistrations ~ TotalChargingUnits + AverageRetailPriceOfElectricity + 
                      RegularGasolinePrice + PurchaseRebate + HOVExemption + ParkingExemption, 
                    data = data_tabBC)
summary(regallbyTypeBC)
bptest(regallbyTypeBC)
# BP = 5.2409, df = 6, p-value = 0.5133
# 0.5133 beaucoup mieux que 0.004214 MAIS ... peut mieux
plot(regallbyTypeBC, sub.caption = "Diagnostic plots pour le modele (2*) avec Box-Cox transformation")
hist(resid(regallbyTypeBC), breaks = 15, main = "Histograms of Residuals", xlab = "Residuals", ylab = "Density")
boxplot(residuals(regallbyTypeBC), main = "Boxplot of Residuals")

#### Correction by Feasible Generalized Least Squared (FGLS)
# comparaison entre nos residus et les White robust standard errors
#library(sandwich)
coeftest(regallbyType2, vcov = vcovHC(regall3, "HC1"))
resiType = regallbyType2$residuals
regVarfuncType = lm(log(resiType^2) ~ log(TotalChargingUnits) + log(AverageRetailPriceOfElectricity) +
                  log(RegularGasolinePrice) + log(PurchaseRebate) + log(ParkingExemption) +
                  log(HOVExemption), data = data_tab)
varfuncType=exp(regVarfuncType$fitted.values)
regallFGLSbyType = lm(PEVRegistrations ~ TotalChargingUnits + AverageRetailPriceOfElectricity + 
                        RegularGasolinePrice + PurchaseRebate + HOVExemption + ParkingExemption, 
                      weights = 1/sqrt(varfuncType), data = data_tab)
summary(regallFGLSbyType)
bptest(regallFGLSbyType)
# BP = 1.0926, df = 5, p-value = 0.9548
# 0.9548 beaucoup mieux que 0.01667 MAIS ... peut peut-être mieux
plot(regallFGLSbyType, sub.caption = "Diagnostic plots pour le modele (2*) avec FGLS Correction")
hist(resid(regallFGLSbyType), breaks = 15, main = "Histograms of Residuals", xlab = "Residuals", ylab = "Density")
boxplot(residuals(regallFGLSbyType), main = "Boxplot of Residuals")

#########################
############ Autocorrelation test 
## Durbin Watson
dwtest(regallbyType2)
# DW = 1.6456, p-value = 0.09258
durbinWatsonTest(regallbyType2)
# lag Autocorrelation D-W Statistic p-value
#  1       0.1637787      1.645641   0.162
### doute ... mais "true autocorrelation is greater than 0"


###################################################################
################################# Modele lineaire General

regallbyTypeGLM = glm(PEVRegistrations ~ TotalChargingUnits + FastChargingUnits + 
                        MedianHouseholdIncome + PercentOfBachelorDegree + AverageRetailPriceOfElectricity + 
                        ResidentialEnergyConsumedPerCapita + RegularGasolinePrice + 
                        IncomeTaxCredit + SalesTax + PurchaseRebate + HOVExemption + 
                        ParkingExemption + EmissionInspection, data=data_tab)
summary(regallbyTypeGLM) 

# selection d'un sous modele optimal
library(MASS)
stepGLM=stepAIC(regallbyTypeGLM, direction="both")
stepGLM$anova
summary(stepGLM)

##########################################################################
############################ Autres méthodes de Selection de Sous-Modele

# Stepwise model selection by exact AIC in the Regression
library(MASS)
######## Actions Publiques toutes regroupees
step=stepAIC(regall, direction="both")
step$anova
summary(step)
######## Actions Publiques toutes separees
stepbyType=stepAIC(regallbyType, direction="both")
stepbyType$anova 
summary(stepbyType)
### coefficients à peu près similaires ...

# All Subsets Regression
library(leaps)
attach(mydata)
leaps=regsubsets(PEVRegistrations ~ TotalChargingUnits + FastChargingUnits + MedianHouseholdIncome + PercentOfBachelorDegree + 
                   AverageRetailPriceOfElectricity + ResidentialEnergyConsumedPerCapita + RegularGasolinePrice + 
                   IncomeTaxCredit + SalesTax + PurchaseRebate + HOVExemption + ParkingExemption + EmissionInspection, 
                 data=data_tab,nbest=10)
# view results 
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
#plot(leaps,scale="r2")
# plot statistic by subset size 
library(car)
subsets(leaps, statistic="rsq")


##########################################################################
############################ Sur la comparaison des predicteurs 

# Calculate Relative Importance for Each Predictor
library(relaimpo)
calc.relimp(regallbyType,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples) 
boot <- boot.relimp(regallbyType, b = 1000, type = c("lmg","last", "first", "pratt"), 
                    rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result


###################################################################
################################# regression log-lineaire multiple

#regallbyTypeLog = regallbyType
#regallbyTypeLog = update(regallbyTypeLog, log(.) ~ log(.))
#summary(regallbyTypeLog)

regallbyTypeLog = lm(log(PEVRegistrations) ~ log(TotalChargingUnits) + log(FastChargingUnits) + 
                        log(MedianHouseholdIncome) + log(PercentOfBachelorDegree) + log(AverageRetailPriceOfElectricity) + 
                        log(ResidentialEnergyConsumedPerCapita) + log(RegularGasolinePrice) + 
                        log(IncomeTaxCredit) + log(SalesTax) + log(PurchaseRebate) + log(HOVExemption) + 
                        log(ParkingExemption) + log(EmissionInspection), data=data_tab)
summary(regallbyTypeLog) 

# selection d'un sous modele optimal selon AIC
#library(MASS)
stepLog=stepAIC(regallbyTypeLog, direction="both")
stepLog$anova
summary(stepLog)


###################################################################
###################################### En plus, a preciser si time

### en enlevant la Californie 

## function of all variables
regallbyTypeNoCal = lm(PEVRegistrations ~ TotalChargingUnits + FastChargingUnits + MedianHouseholdIncome + PercentOfBachelorDegree + AverageRetailPriceOfElectricity + ResidentialEnergyConsumedPerCapita + RegularGasolinePrice 
                  + IncomeTaxCredit + SalesTax + PurchaseRebate + HOVExemption + ParkingExemption + EmissionInspection, data=data_tab[-5,])
summary(regallbyTypeNoCal) 

# selection du meilleur modele inclut dans regall par coeffs de Mallow
library(leaps)
mallowbyTypeNoCal = leaps(x=data_tab[-5,c(5,6,8,11,13,14,15,16,17,18,19,20,21)],y=data_tab[-5,4], 
                     names=colnames(data_tab)[c(5,6,8,11,13,14,15,16,17,18,19,20,21)], method="Cp",nbest=5)
plot(mallowbyTypeNoCal$size,mallowbyTypeNoCal$Cp)
min(mallowbyTypeNoCal$Cp) 
# 3.79545 : le n°21
mallowbyTypeNoCal$which[21,]
# on enleve ce qui est indique : FastChargingUnits, MedianHouseholdIncome, ...
regallbyTypeNoCal2 = regallbyTypeNoCal
regallbyTypeNoCal2 = update(regallbyTypeNoCal2,  ~ .-FastChargingUnits-MedianHouseholdIncome-PercentOfBachelorDegree-AverageRetailPriceOfElectricity-SalesTax-EmissionInspection)
summary(regallbyTypeNoCal2)

# on compare le modele epure choisi a l'original
anova(regallbyTypeNoCal,regallbyTypeNoCal2)

# librairies additionnelles pour regresser par subsets selon un vecteur

#library(nlme)
#lmList(PEVRegistrations ~ TotalChargingUnits + FastChargingUnits + MedianHouseholdIncome + PercentOfBachelorDegree + AverageRetailPriceOfElectricity + ResidentialEnergyConsumedPerCapita + RegularGasolinePrice + TotalPublicActionForEV | clusterIncentives, data=data_tab)
# bof ... donne beaucoup de NA : why ? + peu d'infos
#library(plyr)
#models=dlply(data_tab, clusterIncentives, function(df) lm(PEVRegistrations ~ TotalChargingUnits + FastChargingUnits + MedianHouseholdIncome + PercentOfBachelorDegree + AverageRetailPriceOfElectricity + ResidentialEnergyConsumedPerCapita + RegularGasolinePrice + TotalPublicActionForEV, data = df))
#ldply(models, coef)
#l_ply(models, summary, .print = TRUE)
# bof ... donne beaucoup de NA : why ?
#library(dplyr)
#models = data_tab %>% group_by(Party) %>% do(model = lm(PEVRegistrations ~ TotalChargingUnits + FastChargingUnits + MedianHouseholdIncome + PercentOfBachelorDegree + AverageRetailPriceOfElectricity + ResidentialEnergyConsumedPerCapita + RegularGasolinePrice + TotalPublicActionForEV, data = .))
#models$model
#library(broom)
#models %>% tidy(model)
#models %>% glance(model)
# why not mais juste sur colonnes du data.frame et pas assez précis

