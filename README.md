# EVanalysis

**This project aims to give a statistical and econometric analysis of EV adoption in the US**

Le projet final comprendra :

* une Intoduction visant à présenter notre problématique
* une revue de la littérature existante sur notre sujet
* une description de nos données
* une analyse statistique exploratoire
* une analyse économétrique
* une conclusion quant à nos résultats

### Description of the repository content

Ce projet contient les fichiers suivants :

* README.md : description du projet, des données, des méthodes
* ...

### About our data (2016 values)

__Here is the different data we aim to work on__ : 

- [x] part du parc de EV rapporté à la population (nb pour 1000 hab)
- [ ] montant moyen de l’EV ($)
- [ ] niveau de salaire ($)
- [ ] niveau éducation (années)
- [ ] action publique($) —> gap ou subventions (exemption d’impôt) toutes séparées (plus ou moins
- [ ] prix électricité ($)
- [ ] accès au parking ?
- [ ] nombre de bornes (rapporté à la superficie, ou à la population)
- [ ] nature du possesseur (privé, état, entreprise) ---> analyse de variance

_check when the data has been imported on the repository_


### About ours methods of statistical analysis 

__Here are the diferent data we aim to work on__ : 

- [ ] statistique descriptives: moy et écart type des principales variables
- [ ] analyse en composantes principales (R)
- [ ] méthodes de classification automatique pour représenter les données et émettre des hypothèses de dépendance entre certaines variables --> cluster, k-means (R) --> états plus ou moins écolo : à corréler avec des variables quantitatives (rep, dem, ...) --> test du chi 2 pour voir si même cluster
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


### About ours methods of econometric analysis 

...
