library(ggplot2) # grammaire des graphiques
library(dplyr) # manipulation de données
library(patchwork) # combiner les graphiques
library(MASS) # modèles de régression multinomiale
library(mice) # données manquantes

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

# Comparer avec le modèle qui utilise uniquement les données complètes
# Incertitude plus grande parce qu'on a moins de données (notez le message d'avertissement)
mod_complete <- glm(y ~ .,
    family = binomial,
    data = manquantes)
summary(mod_complete)

# Imputation simple
mod_simple <- glm(
  y ~ .,
  family = binomial,
  data = complete(impdata, action = 1))
summary(mod_simple)
# le modèle fait comme si il n'y avait pas de valeurs manquantes!
# sous-estimation de l'incertitude, les erreurs-types retournées sont trop petites


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
pred <- predict(multi1, type = "class")

# Modèle avec uniquement l'ordonnée à l'origine (~ 1)
# Il faut qu'on utilise les mêmes données pour la comparaison!
multi_cst <- nnet::multinom(catvote ~ 1,
                         weights = poids,
                         data = vote,
                         subset = age > 30,
                         trace = FALSE)
head(predict(multi_cst, type = "prob"), n = 3)
# Test de rapport de vraisemblance
multi0 <- nnet::multinom(
  catvote ~ age  + race + revenu + educ + affiliation,
  data = vote,
  subset = age > 30,
  weights = poids,
  trace = FALSE)
anova(multi0, multi1)
# On peut le faire directement pour chaque variable à tour de rôle
car::Anova(multi1, type = 3)
