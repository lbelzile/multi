## -----------------------------------------------------------------------------
data(factor, package = "hecmulti")
# Analyse en composante principale de la matrice de corrélation
acp <- princomp(factor, cor = TRUE)
# aussi fonction 'prcomp'

# Variance des composantes principales
acp_var <- acp$sdev^2

## -----------------------------------------------------------------------------
# Analyse en composantes principales
# de la matrice de corrélation
data(factor, package = "hecmulti")
acp <- princomp(factor, cor = TRUE)
loadings(acp)  # chargements
biplot(acp,
       xlab = "composante principale 1",
       ylab = "composante principale 2",
       cex = 0.5
       )

# Calcul des composantes principales
mat_cor <- cor(factor)
decompo <- eigen(mat_cor)
# Variances des composantes principales
variances <- decompo$values
# Il faut standardiser les données
factor_std <- as.matrix(scale(factor))
composantes <- factor_std %*% decompo$vectors
# cor(composantes) # corrélations nulles


## -----------------------------------------------------------------------------
hecmulti::eboulis(acp)


## -----------------------------------------------------------------------------
data(factor, package = "hecmulti")
# Représentation graphique de la matrice de corrélation
corrplot::corrplot(cor(factor),
                   type = "upper",
                   diag = FALSE,
                   tl.col = "black")



## -----------------------------------------------------------------------------
facto_cp <- hecmulti::factocp(
      factor,
      nfact = "kaiser")
# nfact: nombre de facteurs ("kaiser" par défaut)
# cor: matrice de corrélation? par défaut vrai


## -----------------------------------------------------------------------------
hecmulti::ajustement_factanal(
    covmat = cor(factor), # matrice de corrélation
    factors = 1:5, # candidats pour nb de facteurs
    n.obs = nrow(factor)) # nombre d'observations


## -----------------------------------------------------------------------------
# Ajuster le modèle factoriel
# par maximum de vraisemblance
fa3 <- factanal(x = factor,
                factors = 3L)
# Imprimer les chargements en
# omettant l'impression des valeurs inférieures à 0.3
print(fa3$loadings,
       cutoff = 0.3)


## -----------------------------------------------------------------------------
# Création des échelles
ech_service <- rowMeans(factor[,c("x4","x8","x11")])
ech_produit <- rowMeans(factor[,c("x3","x6","x9","x12")])
ech_paiement <- rowMeans(factor[,c("x2","x7","x10")])
ech_prix <- rowMeans(factor[,c("x1","x5")])


## -----------------------------------------------------------------------------
# Création des échelles
# Cohérence interne (alpha de Cronbach)
alph <- c(
  hecmulti::alphaC(factor[,c("x4","x8","x11")]),
  hecmulti::alphaC(factor[,c("x3","x6","x9","x12")]),
  hecmulti::alphaC(factor[,c("x2","x7","x10")]),
  hecmulti::alphaC(factor[,c("x1","x5")]))
names(alph) <- c("service","produit","paiement","prix")
alph

