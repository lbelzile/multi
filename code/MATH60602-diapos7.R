## ---- echo = TRUE, eval = TRUE------------------------------------------------
data(dbm, package = "hecmulti")
formule <- formula("yachat ~ x1 + x2 + x3 +
                x4 + x5 + x6 + x7 + x8 + x9 + x10")
modele <- glm(formule,
              data = dbm,
              family = binomial,
              subset = test == 0)


## -----------------------------------------------------------------------------
## set.seed(202209)
## # Option par défaut:
## # validation croisée à 10 plis
## # 10 répétitions
predprob <- hecmulti::predvc(modele)


## -----------------------------------------------------------------------------
data(dbm, package = 'hecmulti')
# Transformer les variables catégorielles en facteurs
# Ne conserver que l'échantillon d'apprentissage
train <- dbm[(dbm$test == 0),]
# Formule pour la moyenne du modèle logistique
form <- formula("yachat ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10")
mod <- glm(data = train,
           formula = form,
           family = binomial)
# Créer un conteneur pour les probabilités
set.seed(60602)
prob <- hecmulti::predvc(mod)
# Histogramme des probabilités prédites par validation croisée avec n groupes
library(ggplot2)
ggplot(data = data.frame(x = prob),
       aes(x = x)) +
  geom_histogram() +
  labs(x = "probabilité d'achat prédite",
       y = "") +
  theme_minimal()
#  Modèle complet et valeurs prédites
train_prob <- fitted(glm(data = train,
                         formula = form,
                         family=binomial),
                     type = "response")

# Performance du modèle avec données d'apprentissage
perfo0 <- perfo(prob = train_prob, resp = train$yachat)
perfo1 <- perfo(prob = prob, resp = train$yachat)


## -----------------------------------------------------------------------------
library(ggplot2)
ggplot(data = perfo1$table,
       aes(x = coupe,
           y = 100 - pcorrect)) +
  geom_line() +
  geom_vline(xintercept = 0.465, linetype = "dashed", alpha = 0.5) +
  labs(x = "point de coupure",
       y = "",
       subtitle = "taux de mauvaise classification (pourcentage)") +
  scale_x_continuous(expand = c(0.01,0.01),
                     breaks = c(0,0.25,0.5,0.75,1),
                     labels = c("0","0.25","0.5","0.75","1")) +
  theme_classic()


## -----------------------------------------------------------------------------
classif <- with(dbm, yachat[test == 0])
# Tableau de la performance
hecmulti::perfo_logistique(
  prob = predprob,
  resp = classif)


## -----------------------------------------------------------------------------
confumat <- perfo1$confusion
rownames(confumat) <- c("\\(\\widehat{Y}=1\\)","\\(\\widehat{Y}=0\\)")
colnames(confumat) <- c("\\(Y=1\\)","\\(Y=0\\)")
knitr::kable(x = confumat,
                align = "r",
                escape = FALSE,
                booktabs = TRUE,
             format = "latex")


## -----------------------------------------------------------------------------
confumat <- perfo1$confusion
rownames(confumat) <- c("\\(\\widehat{Y}=1\\)","\\(\\widehat{Y}=0\\)")
colnames(confumat) <- c("\\(Y=1\\)","\\(Y=0\\)")
confumat_th <- confumat
confumat_th[1:4] <- c("VP","FN","FP","VN")
tab1 <- knitr::kable(x = confumat,
                align = "r",
                escape = FALSE,
                booktabs = TRUE,
             format = "latex")
tab2 <- knitr::kable(x = confumat_th,
                align = "r",
                escape = FALSE,
                booktabs = TRUE,
             format = "latex")
knitr::kables(list(tab1, tab2))



## -----------------------------------------------------------------------------
confumat_tbclassif <- confumat
confumat_tbclassif[1:4] <- c("$c_{11}$","$c_{01}$","$c_{10}$","$c_{00}$")
tab1 <- knitr::kable(x = confumat_tbclassif,
                align = "r",
                escape = FALSE,
                booktabs = TRUE,
             format = "latex")
tab1



## -----------------------------------------------------------------------------
confumat_tbclassif <- confumat
confumat_tbclassif[1:4] <- c("$1$","$0$","$0$","$1$")
tab1 <- knitr::kable(x = confumat_tbclassif,
                align = "r",
                escape = FALSE,
                booktabs = TRUE,
             format = "latex",)
tab1



## -----------------------------------------------------------------------------
data(dbm, package = "hecmulti")
dbm |>
  dplyr::filter(test == 0,
                  !is.na(ymontant))  |>
  dplyr::summarize("n" = length(ymontant),
            "moyenne" = round(mean(ymontant),2),
            "écart-type" =round(sd(ymontant),2),
            "minimum" = min(ymontant),
            "maximum" = max(ymontant)) |>
knitr::kable(booktabs = TRUE) |>
    kableExtra::kable_styling(full_width = TRUE)


## -----------------------------------------------------------------------------
confumat_tbclassif <- confumat
confumat_tbclassif[1:4] <- c("$57$","$0$","$-10$","$0$")
knitr::kable(x = confumat_tbclassif,
                align = "r",
                escape = FALSE,
                booktabs = TRUE,
             format = "latex")


## -----------------------------------------------------------------------------
## formule = formula(yachat ~ x1 + x2 + x3 +
##                     x4 + x5 + x6 + x7 +
##                     x8 + x9 + x10)
## modele <- glm(formule,
##               family = binomial,
##               data = hecmulti::dbm)
## coupe <- hecmulti::select_pcoupe(
##   modele = modele,
##   c00 = 0,
##   c01 = 0,
##   c10 = -10,
##   c11 = 57,
##   plot = TRUE)


## -----------------------------------------------------------------------------
formule = formula(yachat ~ x1 + x2 + x3 +
                    x4 + x5 + x6 + x7 +
                    x8 + x9 + x10)
modele <- glm(formule,
              family = binomial,
              data = hecmulti::dbm)
coupe <- hecmulti::select_pcoupe(
  modele = modele,
  c00 = 0,
  c01 = 0,
  c10 = -10,
  c11 = 57,
  plot = TRUE)


## -----------------------------------------------------------------------------
## roc <- hecmulti::courbe_roc(
##   resp = classif,
##   prob = predprob,
##   plot = TRUE)
## print(roc)
## ## Pour extraire l'aire sous la courbe, roc$aire


## -----------------------------------------------------------------------------
predprob <- predict(cv_glm, type = "prob")[,2]
classif <- with(dbm, yachat[test == 0])
roc <- hecmulti::courbe_roc(
  resp = classif,
  prob = predprob,
  plot = TRUE)
## Pour extraire l'aire sous la courbe, roc$aire


## -----------------------------------------------------------------------------
## tab_lift <- hecmulti::courbe_lift(
##   prob = predprob,
##   resp = classif,
##   plot = TRUE)
## tab_lift


## -----------------------------------------------------------------------------
knitr::kable(tab_lift[,-1],
             booktabs = TRUE,
             digits = 2,
             col.names = c("hasard",
                           "modèle",
                           "lift"),
             format = "latex")


## -----------------------------------------------------------------------------
tab_lift <- hecmulti::courbe_lift(
  prob = predprob,
  resp = classif,
  plot = TRUE)


## -----------------------------------------------------------------------------
hecmulti::calibration(
   prob = predprob,
   resp = classif)

