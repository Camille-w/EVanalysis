#On efface les variables en mémoire, permet de faire le ménage avant de commencer
rm(list=ls())
# ouverture des données
#data_tab=read.table("DataEasyNameOnlyStates.csv",header=TRUE,sep=";")
data_tab=read.table("C:\\Users\\thibaut\\Documents\\Ponts_2A\\Voiture_Electrique\\EVanalysis-master\\EVanalysis-master\\DataEasyNameOnlyStates.csv",header=TRUE,sep=";", dec=",")

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
