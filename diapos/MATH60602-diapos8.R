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
  catvote ~ age + sexe + race + revenu + educ,
  data = vote,     # base de données
  weights = poids, # poids de sondage
  trace = FALSE)   # infos sur convergence


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
multi0 <- nnet::multinom(catvote ~ 1,
                         weights = poids,
                         data = vote,
                         trace = FALSE)
# Test de rapport de vraisemblance
anova(multi0, multi1)

## -----------------------------------------------------------------------------
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
# On peut obtenir les intervalles de Wald
# avec confint.default (PAS RECOMMANDÉ)

# Critères d'information
AIC(multi2a); BIC(multi2a)
# Tableau des coefficients
# Coefficients (variables explicatives)
coef(multi2a)
# Ordonnées à l'origine:
multi2a$zeta


## -----------------------------------------------------------------------------
multi2b <- nnet::multinom(catvote ~ sexe,
  data = vote,  subset = age > 30,
  weights = poids, trace = FALSE)
# Valeur-p du test de rapport de vraisemblance
pchisq(q = deviance(multi2a) - deviance(multi2b),
       df = length(coef(multi2a)),
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
