
## Code pour la sélection de variables
set.seed(60602)
# Diviser les bases de données
# en échantillons d'apprentissage
# et de validation
data(dbm, package = "hecmulti")
valid <- dbm[dbm$test == 1,] |>
  dplyr::select(! c(ymontant, test))
appr <- dbm[dbm$test == 0, ] |>
  dplyr::select(! c(ymontant, test))
# Formule du modèle avec toutes les interactions
# d'ordre 2 (.^2) et les termes quadratiques I(x^2)
formule <- formula(yachat ~ .^2 +
                     I(x2^2) + I(x6^2) +
                     I(x7^2) + I(x8^2) +
                     I(x9^2) + I(x10^2))
# Nouvelles bases de données avec toutes ces variables
# On retire la première colonne (1, ordonnée à l'origine)
appr_c <- data.frame(
  cbind(model.matrix(formule, data = appr)[,-1]),
  y = as.integer(appr$yachat))
valid_c <- data.frame(
  cbind(model.matrix(formule, data = valid)[,-1]),
  y = as.integer(valid$yachat))
valid_ymontant <- with(dbm, ymontant[test == 1L])

# Ajustement des différents modèles

# Modèle avec toutes les variables principales
base <- glm(yachat ~ .,
            data = appr,
            family = binomial)
# Calcul du point de coupe optimal
#  (par validation croisée)
base_coupe <- hecmulti::select_pcoupe(
  modele = base,
  c00 = 0,
  c01 = 0,
  c10 = -10,
  c11 = 57)
# Performance sur données de validation
base_pred <-
  predict(object = base,
          newdata = valid,
          type = "response") > base_coupe$optim
base_perfo <-
  -10*sum(base_pred) +
  sum(valid_ymontant[base_pred], na.rm = TRUE)

# Modèle avec toutes les variables + interactions
# Ajustement
complet <- glm(formula = formule,
               data = appr,
               family = binomial)
# Sélection du point de coupure
complet_coupe <- hecmulti::select_pcoupe(
  modele = complet, c00 = 0,
  c01 = 0, c10 = -10, c11 = 57)
# Performance sur données de validation
complet_pred <-
  predict(object = complet,
          newdata = valid,
          type = "response") > complet_coupe$optim
# Revenu
complet_perfo <-
  -10*sum(complet_pred) +
  sum(valid_ymontant[complet_pred], na.rm = TRUE)

# Sélection de modèle avec algorithme glouton
# Recherche séquentielle (AIC)
seqAIC <- step(object = complet,
               direction = "both", # séquentielle
               k = 2, # AIC
               trace = 0)
seqAIC_coupe <-
  hecmulti::select_pcoupe(
    modele = seqAIC, c00 = 0,
    c01 = 0, c10 = -10, c11 = 57)
seqAIC_pred <-
  predict.glm(object = seqAIC,
              newdata = valid,
              type = "response") >
  seqAIC_coupe$optim
seqAIC_perfo <-
  -10*sum(seqAIC_pred) +
  sum(valid_ymontant[seqAIC_pred],
      na.rm = TRUE)
# Recherche séquentielle (BIC)
seqBIC <- step(object = complet,
               direction = "both", # séquentielle
               k = log(nobs(complet)), #BIC
               trace = 0)
seqBIC_coupe <- hecmulti::select_pcoupe(
  modele = seqBIC, c00 = 0,
  c01 = 0, c10 = -10, c11 = 57)
seqBIC_pred <-
  predict.glm(object = seqBIC,
              newdata = valid,
              type = "response") >
  seqBIC_coupe$optim
seqBIC_perfo <-
  -10*sum(seqBIC_pred) +
  sum(valid_ymontant[seqBIC_pred],
      na.rm = TRUE)

# Recherche exhaustive par algorithm génétique
# avec moins de variables
appr_r <- data.frame(
  cbind(model.matrix(seqAIC)[,-1],
        y = appr$yachat))
valid_r <- data.frame(
  model.matrix(formula(seqAIC),
               data = valid)[,-1])
library(glmulti)
exgen <- glmulti::glmulti(
  y = y ~ .,
  #nombre de variables limitées
  data = appr_r,
  level = 1,           # sans interaction
  method = "g",        # recherche génétique
  crit = "bic",            # critère (AIC, BIC, ...)
  confsetsize = 1,         # meilleur modèle uniquement
  plotty = FALSE,
  report = FALSE,  # sans graphique ou rapport
  fitfunction = "glm")
# Redéfinir le modèle via "glm"
exgen_modele <-
  glm(exgen@objects[[1]]$formula,
      data = appr_r,
      family = binomial)
exgen_coupe <-
  hecmulti::select_pcoupe(
    modele = exgen_modele,
    c00 = 0, c01 = 0, c10 = -10, c11 = 57)
exgen_pred <-
  predict(exgen_modele,
          newdata = valid_r,
          type = "response") > exgen_coupe$optim
exgen_perfo <-
  -10*sum(exgen_pred) +
  sum(valid_ymontant[exgen_pred],
      na.rm = TRUE)

# LASSO
# Trouver le paramètre de pénalisation par
# validation croisée (10 groupes)
cvfit <- glmnet::cv.glmnet(
  x = as.matrix(appr_c[, -ncol(appr_c)]),
  y = appr_c$y,
  family = "binomial",
  type.measure = "auc") # aire sous courbe
# Le critère par défaut est la déviance (-2ll)
# Ajuster modèle avec pénalisation
lasso <- glmnet::glmnet(
  x = as.matrix(appr_c[,-ncol(appr_c)]),
  y = appr_c$y,
  family = "binomial",
  lambda = cvfit$lambda.1se)
# Calculer performance selon les points de coupure
probs_lasso <-
  predict(lasso,
          newx = as.matrix(appr_c[,-ncol(appr_c)]),
          type = "resp")
lasso_coupe <- with(
  hecmulti::perfo_logistique(
    prob = probs_lasso,
    resp = appr_c$y),
  coupe[which.max(VP*57 - FN*10)])
lasso_pred <- c(predict(lasso,
                        newx = as.matrix(valid_c[,-ncol(valid_c)]),
                        type = "resp")) > lasso_coupe
lasso_perfo <- -10*sum(lasso_pred) +
  sum(valid_ymontant[lasso_pred], na.rm = TRUE)

classif <- rbind(
  c(table(base_pred, valid$yachat)),
  c(table(complet_pred, valid$yachat)),
  c(table(seqAIC_pred, valid$yachat)),
  c(table(seqBIC_pred, valid$yachat)),
  c(table(exgen_pred, valid$yachat)),
  c(table(lasso_pred, valid$yachat)))
colnames(classif) <- c("VN","FN","FP","VP")
sensibilite <- c(1, classif[,"VP"]/(classif[,"VP"] + classif[,"FN"]))
tauxbonneclassif <- c(0.232, (classif[,"VP"] + classif[,"VN"])/rowSums(classif))

datf <- data.frame(
  modele = paste0("(", letters[1:7], ")"),
  ncoef = c(NA,
            length(coef(base)),
            length(coef(complet)),
            length(coef(seqAIC)),
            length(coef(seqBIC)),
            length(coef(exgen_modele)),
            lasso$df + 1L) - 1L, # retirer l'ordonnée à l'origine
  pcoupe = c(NA,
             base_coupe$optim,
             complet_coupe$optim,
             seqAIC_coupe$optim,
             seqBIC_coupe$optim,
             exgen_coupe$optim,
             lasso_coupe),
  classif = tauxbonneclassif,
  sensibilite = sensibilite,
  gain = c(601212,
           base_perfo,
           complet_perfo,
           seqAIC_perfo,
           seqBIC_perfo,
           exgen_perfo,
           lasso_perfo))
# Imprimer le tableau
knitr::kable(datf,
             col.names =  c("modèle",
                            "no. variables",
                            "pt. coupure",
                            "sensibilité",
                            "taux bonne classif.",
                            "profit"),
             row.names = FALSE,
             booktabs = TRUE,
             longtable = FALSE,
             align =  paste0(c("l",rep("r", 5)),
                             collapse = ""),
             round = c(0, 0, 2, 2, 3, 0),
             escape = FALSE)
