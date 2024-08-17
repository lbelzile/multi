## Le paquet hecmulti contient plusieurs fonctions pour l'analyse factorielle
## Charger le paquet
library(hecmulti)
## Charger les données
data(factor, package = "hecmulti")
# Pour faire une analyse en composantes principales,
# on calcule la matrice de corrélation cor() ou de covariance cov()
# et on fait une décomposition en valeurs propres/vecteurs propres

# Calcul des composantes principales
mat_cor <- cor(factor)
decompo <- eigen(mat_cor)
# valeurs propres = variances des composantes principales
variances <- decompo$values
# Il faut standardiser les données si on travaille avec les corrélations
factor_std <- as.matrix(scale(factor))
# On multiplie la matrice d'observations par la matrice changement de base
# donnée par les vecteurs propres -> nouvelles coordonnées
# (qu'on appelle les composantes principales)
composantes <- factor_std %*% decompo$vectors
## On peut vérifier que les nouvelles variables/coordonnées
## ont une corrélation de 0
cor(composantes) # corrélations nulles
## Les variances estimées sont les mêmes que celles dans 'variance'
diag(cov(composantes))

# Dans R, on peut aussi utiliser directement les fonctions pour
# l'analyse en composante principale de la matrice de corrélation
acp <- princomp(factor, cor = TRUE)
# aussi fonction 'prcomp'
loadings(acp)  # chargements
acp$scores # coordonnées des composantes principales
# bigramme
biplot(acp,
       xlab = "composante principale 1",
       ylab = "composante principale 2",
       cex = 0.5
       )
# les directions des flèches sont données par les chargements
# les coordonnées des points sont celles des deux premières
# composantes principales

# la liste de retour contient les écarts-types des c.p.
isTRUE(all.equal(variances, acp$sdev^2, check.attributes = FALSE))

## Choix du nombre de facteurs
# critère des valeurs propres (Kaiser) > 1
sum(variances > 1)
## Diagramme d'éboulis
# graphique du nombre de composantes (x)
# en fonction des variances (y = acp_var)
hecmulti::eboulis(acp)
# Le point d'inflection est à 5
# Pourcentage de variances cumulatives
cumsum(variances)/sum(variances)
# la variance totale est la somme des variances
isTRUE(all.equal(sum(variances), sum(diag(mat_cor))))

# Représentation graphique de la matrice de corrélation
corrplot::corrplot(cor(factor),
                   type = "upper",
                   diag = FALSE,
                   tl.col = "black")

## Analyse factorielle
facto_cp <- hecmulti::factocp(
      factor,
      nfact = "kaiser")
# nfact: nombre de facteurs ("kaiser" par défaut)
# cor: matrice de corrélation? par défaut vrai


## Ajustement du modèle avec
hecmulti::ajustement_factanal(
    covmat = cor(factor), # matrice de corrélation
    factors = 1:5, # candidats pour nb de facteurs
    n.obs = nrow(factor)) # nombre d'observations
## Le test d'hypothèse (valeur p, colonne 'pval')
# compare H1: corrélation non structurée (une corrélation par paire)
# vs la matrice de corrélation découlant de l'analyse factorielle



## -----------------------------------------------------------------------------
# Ajuster le modèle factoriel
# par maximum de vraisemblance
fa3 <- factanal(x = factor,
                factors = 3L)
# Imprimer les chargements en
# omettant l'impression des valeurs inférieures à 0.3
print(fa3$loadings,
       cutoff = 0.3)

# Créations des échelles et calcul du alpha de Cronbach
echelles <- creation_echelles(
  chargements = fa3$loadings,
  data = factor,
  seuil = 0.4
)
# Valeurs du alpha
echelles$alpha

## La même chose, mais avec un calcul manuel
# Création des échelles
ech_service <- rowMeans(factor[,c("x4","x8","x11")])
ech_produit <- rowMeans(factor[,c("x3","x6","x9","x12")])
ech_paiement <- rowMeans(factor[,c("x2","x7","x10")])
ech_prix <- rowMeans(factor[,c("x1","x5")])

# Création des échelles
# Cohérence interne (alpha de Cronbach)
alpha <- c(
  hecmulti::alphaC(factor[,c("x4","x8","x11")]),
  hecmulti::alphaC(factor[,c("x3","x6","x9","x12")]),
  hecmulti::alphaC(factor[,c("x2","x7","x10")]),
  hecmulti::alphaC(factor[,c("x1","x5")]))
names(alpha) <- c("service","produit","paiement","prix")
alpha
# On voit que la valeur de alpha pour le prix ne satisfait pas le critère de cohérence interne.
