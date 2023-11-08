library(mice)

data(manquantes, package = 'hecmulti')
summary(manquantes)
# Pourcentage de valeurs manquantes
# Fonction apply: appliquer une fonction
# 2 signifie qu'on veut conserver les colonnes
# Ici, on calcule la proportion de valeurs manquantes
# variables par variables
apply(manquantes, 2, function(x){mean(is.na(x))})
# Voir les configurations de valeurs manquantes
md.pattern(manquantes) 

# Intensif en calcul, réduire "m" si nécessaire
impdata <- mice(data = manquantes,
# argument method pour le type de modèles
# selon les variables
m = 50, # nombre d'imputations
seed = 60602, # germe aléatoire
printFlag = FALSE) # ne pas imprimer le suivi
# Chaque copie est disponible (1, ..., 50)
complete(impdata, action = 1)
# ajuste le modèle avec les données imputées
adj_im <- with(
  data = impdata,
  expr = glm(y ~ x1 + x2 + x3 + x4 + x5 + x6,
             family = binomial))
# combinaison des résultats 
fit <- pool(adj_im)
# Tableau résumé avec valeurs-p, degrés de liberté et erreur-types corrigées
summary(fit)

## -----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(patchwork)
data(vote, package = "hecmulti")

## -----------------------------------------------------------------------------
data(vote, package = "hecmulti")
levels(vote$catvote)
# Modèle multinomial
multi1 <- nnet::multinom(
  catvote ~ age + sexe + race + revenu + educ + affiliation,
  data = vote,       # base de données
  subset = age > 30, # sous-ensemble
  weights = poids,   # poids de sondage
  trace = FALSE)     # infos sur convergence


## -----------------------------------------------------------------------------
# Tableau résumé de l'ajustement
summary(multi1)
# Estimations des coefficients
coef(multi1)
# Intervalles de confiance (Wald)
confint(multi1)
# Critères d'information
AIC(multi1)
BIC(multi1)
# Prédiction: probabilité de chaque modalité
predict(multi1, type = "probs")
# Prédiction: classe la plus susceptible
predict(multi1, type = "class")


## -----------------------------------------------------------------------------
# Modèle avec uniquement l'ordonnée à l'origine (~ 1)
# Il faut qu'on utilise les mêmes données pour la comparaison!
multi0 <- nnet::multinom(catvote ~ 1,
                         weights = poids,
                         data = vote,
                         subset = age > 30,
                         trace = FALSE)
# Test de rapport de vraisemblance
anova(multi0, multi1)

## -----------------------------------------------------------------------------
# Modèle logistique à cote proportionnelle
with(vote, is.ordered(catvote))
multi2a <- MASS::polr(
  catvote ~ sexe,
  data = vote,
  subset = age > 30,
  weights = poids,
  method = "logistic",
  Hess = TRUE)
summary(multi2a)


-----------------------------------------------------------------------------
# IC pour beta_x (vraisemblance profilée)
confint(multi2a)


# Critères d'information
AIC(multi2a); BIC(multi2a)
# Tableau des coefficients
# Coefficients (variables explicatives)
coef(multi2a)
# Ordonnées à l'origine:
multi2a$zeta


## -----------------------------------------------------------------------------
multi2b <- nnet::multinom(
   catvote ~ sexe,
   data = vote,  
   subset = age > 30,
   weights = poids, 
   trace = FALSE)
   
# Combien de paramètres de plus avec modèle logistique?
# Même nombre d'ordonnées à l'origine, mais pour les p variables explicatives
# (K-1)*p vs p  paramètres, une différence de (K-2) * p 
difddl <- (length(multi2a$zeta) - 1) * length(coef(multi2a))
# Valeur-p du test de rapport de vraisemblance
# Probabilité qu'une variable khi-deux avec ddl(0)-ddl(1)
#  dépasse la valeur numérique de la statistique
pchisq(q = deviance(multi2a) - deviance(multi2b),
       df = difddl,
       lower.tail = FALSE)


## -----------------------------------------------------------------------------
multi3a <- MASS::polr(
  catvote ~ age,
  data = vote,
  subset = age > 30,
  weights = poids,
  method = "logistic",
  Hess = TRUE)
multi3b <- nnet::multinom(catvote ~ age,
  data = vote,  subset = age > 30,
  weights = poids, trace = FALSE)
# Valeur-p du test de rapport de vraisemblance
pval <- pchisq(q = deviance(multi3a) - deviance(multi3b),
       df = length(coef(multi2a)),
       lower.tail = FALSE)


## -----------------------------------------------------------------------------
library(MASS)
multi3a <- MASS::polr(
  catvote ~ scale(age, scale = FALSE),
  data = vote,
  subset = age > 30,
  weights = poids,
  method = "logistic",
  Hess = TRUE)

multi3b <- nnet::multinom(
  catvote ~ scale(age, scale = FALSE),
  data = vote,
  subset = age > 30,
  weights = poids,
  Hess = TRUE,
  trace = FALSE)
xpred <- seq(30, 95, by = 0.1) - mean(vote$age)
nobs <- length(xpred)
pred1 <- predict(multi3b,
                 newdata = data.frame(age = xpred),
                 type = "prob")
pred2 <- predict(multi3a,
                 newdata = data.frame(age = xpred),
                 type = "prob")
