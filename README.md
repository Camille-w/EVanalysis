# EVanalysis

**This project aims to give a statistical and econometric analysis of EV adoption in the US**

Le projet final comprendra :

- [x]  une intoduction visant à présenter notre problématique
- [x] une revue de la littérature existante sur notre sujet
- [x] une description de nos données
- [x] une analyse statistique exploratoire
- [x] une analyse économétrique(+ test hypothèses)
- [x] une conclusion quant à nos résultats

### Description of the repository content

Ce projet contient les fichiers suivants (en gras les fichiers clés) :

* AllData.csv : data table (by state, in 2016) comportant les données cochées ci après 
* Biplot.png : le Biplot associé à la PCA (etat, et vecteuc valeur)
* Cluster00i.png : résultat du programme de clustering en image automatiquement générées
* CompareCluster00i.png : résultat des comparaisons de clustering en image automatiquement générées
* DataEasyName.csv : la table AllData.csv avec de noms de colonnes utilisables pour R (espaces, ...)
* __DataEasyNameOnlyStates.csv : DataEasyName.csv sans la valeur pour USA en totalite__
* DataEasyNameRenorm.csv : les donnees renormalisees manuellement (inutile car scale)
* __Description_classique.R : resumé descriptif et histogrammes avec densité__
* EVanalysis.Rproj : groupement en projet R des fichiers du repertoire
* individualsPCA.png : la PCA avec les noms des etas, coloree par niveau de représentation
* pca.R : analyses des donnees de type PCA, (normalisee automatiquement, selection des colonnes)
* PCA00i.png : résultat du programme de PCA en image automatiquement générées
* __pcaCluster.R : PCA normalisee et plusieurs analyse de clustering (avec leur pertinence) et comparaison aux cluster de parti, et test du chi2 homogénéité et indépendance entre cluster ...__
* __plot.R__ : affichage du scatter plot, d'un résumé des données via GGally et de plusieurs regressions "interessantes"
* __plotData.png : scatter plot des donnees__
* __radar.R : tracé des diagrammes radar (ou en étoile) pour chacun de états (plusieurs par fenetres)__
* __README.md : description du projet, des données, des méthodes__
* __regression.R : regressions lineaire multiples sur plusieurs modeles (2) avec modele et ses resultats, election du sous modele, test des hypotheses MCO, test de chow, correction et analyse des corrections__
* regressiontesthyp.R : quelques test sur les hypothèses MCO présentés séparemment

### About our data (2016 values)

__Here is the different data we aim to work on__ : 

- [x] (population de l'état)
- [x] part du parc de EV rapporté à la population (nb pour 1000 hab)
- [x] niveau de salaire ($) ; 2 data : personal income per capita et mean household income ... et leur rapport
- [x] niveau éducation ; 3 data : % de plus de 25 ans ayant atteint un certain niveau de diplome (High School, Bachelor, Advanced)
- [x] prix moyen de vente de l'électricité (cents/kWh)
- [x] nombre de bornes (nb d'unité de charge pour 1000 hab) ; 2 data : fast charging units only ; Level II AND fast charging units
- [x] action publique($) —> séparé selon le type de mesure et avec le total (valuation selon l'EIA)
- [x] parti politique le plus souvent voté récemment
- [x] résultat à des tests sur l'implication écologique des états
- [ ] nature du possesseur (privé, état, entreprise) ? ---> analyse de variance
- [ ] prix moyen d'achat d'un EV dans l'état

_check when the data has been imported on the repository_


### About ours goals for the statistical and econometric analysis 

__Here are the different methods we implement (dans l'ordre)__ : 

- [x] statistique descriptives: moy, quantiles, médiane, écart type, ... des principales variables
- [x] statistique descriptives: histogramme avec densité des principales variables 
- [x] statistique descriptives: histogramme avec densité des principales variables regroupé par parti
- [ ] statistique descriptives: histogramme avec densité des principales variables regroupé par cluster kmeans
- [x] diagramme radar (en étoile) pour comparer visuellement les états

- [x] analyse en composantes principales (R)
- [x] méthodes de classification automatique pour représenter les données et émettre des hypothèses de dépendance entre certaines variables --> cluster, k-means (R) 
- [x] états plus ou moins écolo : corréler les cluster avec des variables quantitatives (rep, dem, ...)
- [x] PCA et clustering sur l'action publique
- [x] test du chi 2 pour voir si même cluster (definis et obtenus)

- [x] regressions lineaires multiples sur plusieurs modeles
- [x] determination du sous modele "ideal" de regression lineaire multiple (par modele)
- [x] test structurels par cluster (ex: selon le parti politique) (par modele)
- [x] test des hypotheses des MCO (homoskédasticité, autocorrélation, significativité globale, ...) de plusieurs facons (par modele) 
- [x] tentatives de correction (par sous modele ideal)
- [x] tests sur les corrections (par sous modele ideal)
- [x] regression lineaire generale sur un modele
- [x] regression log lineaire sur un modele

__Here are possible prospect methods__ : 

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
- [x] Trouver des sources de documents
- [x] Resumer les differentes methodes mises en oeuvre (théorier et pratique)
- [x] Comparer les résultats extérieurs aux notres
