## -----------------------------------------------------------------------------
data(factor, package = "hecmulti")
acp <- princomp(factor, cor = TRUE)
# aussi fonction 'prcomp'

acp_var <- t(formatC(round(acp$sdev^2,3), digits = 2,
                          format = "f")
                  )
colnames(acp_var) <- paste0("C",1:12)
kableExtra::kbl(t(acp_var[,1:8]), 
                align = "c",
                format = "latex",
                row.names = FALSE,
                booktabs = TRUE,
                caption = NULL) #|>
 # kableExtra::kable_styling(full_width = TRUE)



## -----------------------------------------------------------------------------
Sigma <- diag(c(4,1)) %*% cbind(c(1,0.9),c(0.9,1))
set.seed(123456)
data <- rbind(MASS::mvrnorm(n = 200, 
                      mu = c(4,4), 
                      Sigma = Sigma),
                MASS::mvrnorm(n = 50, 
                              mu = c(-4,-2), 
                              Sigma = Sigma))
pc <- princomp(data, cor = FALSE)
data <- data.frame(x1 = data[,1],
                   x2 = data[,2])
pcdata <- data.frame(x1 = pc$scores[,1],
                     x2 = pc$scores[,2])
library(ggplot2)
library(patchwork)
g1 <- ggplot(data = data, aes(x = x1, y = x2)) +
  geom_point(alpha = 0.8) + 
  geom_abline(slope = pc$loadings[1,2], 
              intercept = pc$loadings[1,1]) + 
  geom_abline(slope = pc$loadings[2,2], 
              intercept = pc$loadings[2,1],
              linetype = 2) + 
  ggpubr::stat_cor(aes(label = ..rr.label..),
                   r.accuracy = 0.01,
                   label.x = 3, 
                   label.y = 0) +
  ylim(-10,10) + 
  xlim(-10,10) +
  labs(x = "variable 1",
       y = "variable 2") + 
  coord_fixed() +
  theme_classic()
g2 <- ggplot(data = pcdata, aes(x = x1, y = x2)) +
  geom_point(alpha = 0.8) +
  ggpubr::stat_cor(aes(label = ..rr.label..),
                   r.accuracy = 0.01,
                   label.x = -10, 
                   label.y = -2) +
  labs(x = "composante principale 1",
       y = "composante principale 2") +
  theme_classic()

g1 + g2


## -----------------------------------------------------------------------------
## # Analyse en composantes principales
## # de la matrice de corrélation
## acp <- princomp(factor, cor = TRUE)
## loadings(acp) # chargements
## biplot(acp) # bigramme


## -----------------------------------------------------------------------------
# Analyse en composantes principales
# de la matrice de corrélation
data(factor, package = "hecmulti")
acp <- princomp(factor, cor = TRUE)
# loadings(acp)
biplot(acp, 
       xlab = "composante principale 1",
       ylab = "composante principale 2",
       cex = 0.5
       )


## -----------------------------------------------------------------------------
## hecmulti::eboulis(acp)


## -----------------------------------------------------------------------------
hecmulti::eboulis(acp)


## -----------------------------------------------------------------------------
data(factor, package = "hecmulti")
corrplot::corrplot(cor(factor), 
                   type = "upper", 
                   diag = FALSE,
                   tl.col = "black")


## -----------------------------------------------------------------------------
data(factor, package = "hecmulti")
facto_cp <- hecmulti::factocp(factor)
chargements <- facto_cp$loadings
class(chargements) <- "matrix"
chargements[abs(chargements) < 0.3] <- NA
colnames(chargements) <- paste0("F", 1:4)
rownames(chargements) <- paste0("x",1:12)
options(knitr.kable.NA = '')
knitr::kable(100*chargements, 
                digits = 0,
                booktabs = TRUE,
                row.names = TRUE)


## -----------------------------------------------------------------------------
## library(hecmulti)
## facto_cp <- factocp(factor,
##                     nfact = "kaiser",
##                     cor = TRUE)
## # nfact: nombre de facteurs ("kaiser" par défaut)
## # cor: matrice de corrélation? par défaut vrai


## -----------------------------------------------------------------------------
## library(hecmulti)
## ajustement_factanal(
##     covmat = cor(factor), # matrice de corrélation
##     factors = 1:5, # candidats pour nb de facteurs
##     n.obs = nrow(factor)) # nombre d'observations


## -----------------------------------------------------------------------------
emv_crit <- hecmulti::ajustement_factanal(
    covmat = cov(factor),
    factors = 1:5,
    n.obs = nrow(factor))
colnames(emv_crit)[4] <- "valeur-p"
knitr::kable(emv_crit, 
                digits = c(2,2,2,0,0),
                row.names = FALSE,
                booktabs = TRUE)


## -----------------------------------------------------------------------------
## # Ajuster le modèle factoriel
## # par maximum de vraisemblance
## fa3 <- factanal(x = factor,
##                 factors = 3L)
## # Imprimer les chargements en
## # omettant les valeurs inférieures à 0.3
## print(fa3$loadings,
##       cutoff = 0.3)


## -----------------------------------------------------------------------------
data(factor, package = 'hecmulti')
fa3 <- factanal(x = factor, 
                factors = 3L)
chargements <- fa3$loadings
class(chargements) <- "matrix"
chargements[chargements < 0.3] <- NA
colnames(chargements) <- paste0("F", 1:3)
options(knitr.kable.NA = '')
knitr::kable(100*chargements, 
                digits = 0,
                booktabs = TRUE,
                row.names = TRUE)


## -----------------------------------------------------------------------------
## # Création des échelles
## ech_service <- rowMeans(factor[,c("x4","x8","x11")])
## ech_produit <- rowMeans(factor[,c("x3","x6","x9","x12")])
## ech_paiement <- rowMeans(factor[,c("x2","x7","x10")])
## ech_prix <- rowMeans(factor[,c("x1","x5")])


## -----------------------------------------------------------------------------
## alphaC(factor[,c("x4","x8","x11")])
## alphaC(factor[,c("x3","x6","x9","x12")])
## alphaC(factor[,c("x2","x7","x10")])
## alphaC(factor[,c("x1","x5")])


## -----------------------------------------------------------------------------
# Création des échelles
# Cohérence interne (alpha de Cronbach)
alph <- c(
  hecmulti::alphaC(factor[,c("x4","x8","x11")]),
  hecmulti::alphaC(factor[,c("x3","x6","x9","x12")]),
  hecmulti::alphaC(factor[,c("x2","x7","x10")]),
  hecmulti::alphaC(factor[,c("x1","x5")]))
names(alph) <- c("service","produit","paiement","prix")
knitr::kable(t(alph), 
             digits = 3, 
             booktabs = TRUE,
             row.names = FALSE) 

