# EVanalysis

**This project aims to give a statistical and econometric analysis of EV adoption in the US**

Le projet final comprendra :

* une intoduction visant à présenter notre problématique
* une revue de la littérature existante sur notre sujet
* une description de nos données
* une analyse statistique exploratoire
* une analyse économétrique(+ test hypothèses)
* une conclusion quant à nos résultats

### Description of the repository content

Ce projet contient les fichiers suivants :

* README.md : description du projet, des données, des méthodes
* AllData.csv : data table (by state, in 2016) comportant les données cochées ci après 
* Biplot.png : le Biplot associé à la PCA (etat, et vecteu valeur)
* DataEasyName.csv : la table AllData.csv avec de noms de colonnes utilisables pour R (espaces, ...)
* DataEasyNameOnlyStates.csv : DataEasyName.csv sans la valeur pour USA en totalite
* DataEasyNameRenorm.csv : les donnees RENORMALISEES (meme ordre de grandeur a peu pres) et noms utilisables
* EVanalysis.Rproj : groupement en projet R des fichiers du repertoire
* individualsPCA.png : la PCA avec les noms des etas, coloree par niveau de représentation
* pca.R : analyses des donnees de type PCA, (normalisee automatiquement, selection des colonnes)
* pcaCluster.R : PCA normalisee et plusieurs analyse de clustering (avec leur pertinence)
* plot.R : affichage du scatter plot et de plusieurs regressions "interessantes"
* plotData.png : scatter plot des donnees
* ...

### About our data (2016 values)

__Here is the different data we aim to work on__ : 

- [x] (population de l'état)
- [x] part du parc de EV rapporté à la population (nb pour 1000 hab)
- [x] niveau de salaire ($) ; 2 data : personal income per capita et mean household income ... et leur rapport
- [x] niveau éducation ; 3 data : % de plus de 25 ans ayant atteint un certain niveau de diplome (High School, Bachelor, Advanced)
- [x] prix moyen de vente de l'électricité (cents/kWh)
- [x] nombre de bornes (nb d'unité de charge pour 1000 hab) ; 2 data : fast charging units only ; Level II AND fast charging units
- [x] action publique($) —> séparé selon le type de mesure et avec le total (valuation selon l'EIA)
- [ ] nature du possesseur (privé, état, entreprise) ? ---> analyse de variance

_check when the data has been imported on the repository_


### About ours goals for the statistical and econometric analysis 

__Here are the diferent methods we aim to implement __ : 
- [x] statistique descriptives: moy et écart type des principales variables
- [x] analyse en composantes principales (R)
- [x] méthodes de classification automatique pour représenter les données et émettre des hypothèses de dépendance entre certaines variables --> cluster, k-means (R) 
- [ ] états plus ou moins écolo : corréler les cluster avec des variables quantitatives (rep, dem, ...) --> test du chi 2 pour voir si même cluster
- [ ] test d'adéquation des données à une loi ou à une famille de loi (selon la nature des données, test du chi-2, tests de normalité, test de Kolmogorov) (juste pour une partie not. facteurs sociaux)
- [ ] estimation des paramètres par intervalles de confiance --> surtout pour les résultat de la régression linéaire --> 50 var assez
- [ ] analyse de variance à un facteur pour des échantillons multiples --> hypothese gaussienne ou test d'homogénéité non paramétrique mais avec seulement 2 types de données (sur le type de flotte)
- [ ] test d'indépendance (chi-2, test du rapport de vraisemblance dans la régression logistique) (Y or N ?)
- [ ] tests d'homogénéité entre deux échantillons (test de Student sous hypothèse gaussienne, test de Kolmogorov-Smirnov dans le cas général)
- [ ] test d'égalité de moyennes
- [ ] test de fischer exact
- [ ] estimation de coefficients dans une régression linéaire ou logistique
- [ ] penser au test d'égalité des variances si le modèle nécessite cette hypothèse (Y or N ?)
- [ ] penser à ajuster les niveaux de test si des tests multiples sont effectués

_check when the method have been applied through one of the repository files_


### About the revue of existing paper on the subject 

__Here are the diferent goals we aim to work on__ : 
- [ ] Trouver des sources de documents
- [ ] Resumer les differentes methodes mises en oeuvre (théorier et pratique)
- [ ] Comparer les résultats extérieurs aux notres
