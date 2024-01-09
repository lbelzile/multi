data(dbm, package = 'hecmulti')
# Transformer les variables catégorielles en facteurs si nécessaire
# Ne conserver que l'échantillon des données d'entraînement (1000 obs)
train <- dbm[(dbm$test == 0),]
# Extraire le vecteur contenant la variable réponse pour un usage ultérieur
classif <- train$yachat

# Formule pour la moyenne du modèle logistique
form <- formula("yachat ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10")
# Modèle de base avec variables (sans interaction)
modele <- glm(
  data = train,
  formula = form,
  family = binomial) # modèle logistique, fn de liaison logistique par défaut
# Créer un conteneur pour les probabilités
set.seed(60602)
## # Option par défaut:
## # validation croisée à 10 plis avec 10 répétitions
## les probabilités prédites sont les moyennes des 10
cv_prob <- hecmulti::predvc(modele, K = 10, nrep = 10)

# Histogramme des probabilités prédites par validation croisée avec n groupes
library(ggplot2)
ggplot(data = data.frame(x = cv_prob),
       aes(x = x)) +
  geom_histogram(bins = 30) +
  labs(x = "probabilité d'achat prédite",
       y = "") +
  theme_minimal()
#  Modèle complet et valeurs prédites
train_prob <- fitted(modele,
                     type = "response")

# Performance du modèle avec données d'apprentissage

# Probabilités calculées avec données ajustées
perfo0 <- hecmulti::perfo_logistique(
  prob = train_prob,
  resp = train$yachat)
# Probabilités obtenues par validation croisée (mieux, plus fiable)
perfo1 <- hecmulti::perfo_logistique(
  prob = cv_prob,
  resp = train$yachat)
# Point de coupure qui maximise le taux de bonne classification
coupe_opt <- perfo1$coupe[which.max(perfo1$pcorrect)]

## -----------------------------------------------------------------------------
## Diagramme montrant le taux de bonne classification
library(ggplot2)
ggplot(data = perfo1,
       aes(x = coupe,
           y = pcorrect)) +
  geom_line() +
  geom_vline(xintercept = coupe_opt, linetype = "dashed", alpha = 0.5) +
  labs(x = "point de coupure",
       y = "",
       subtitle = "taux de bonne classification (pourcentage)") +
  scale_x_continuous(expand = c(0.01,0.01),
                     breaks = c(0,0.25,0.5,0.75,1),
                     labels = c("0","0.25","0.5","0.75","1")) +
  theme_classic()

## -----------------------------------------------------------------------------
## Tableaux de classification pour chaque point de coupure
tab0 <- knitr::kable(x = perfo1[,1:5],
                     align = "r",
                     escape = FALSE,
                     booktabs = TRUE)
tab0

# Point de coupure optimal avec fonction de coût
coupe <- hecmulti::select_pcoupe(
  modele = modele,
  c00 = 0,
  c01 = 0,
  c10 = -10,
  c11 = 57,
  plot = TRUE)
# Le graphique produit représente le gain individuel moyen

## -----------------------------------------------------------------------------
roc <- hecmulti::courbe_roc(
   resp = classif,
   prob = cv_prob,
   plot = TRUE)
print(roc)
## ## Pour extraire l'aire sous la courbe, roc$aire


## -----------------------------------------------------------------------------
## Tableau lift
## La fonction calcule le lift et
## retourne un graphique (plot = TRUE)
## ainsi que le tableau pour les déciles
## pourcentage détecté si on classe 10% plus élevé
## en succès comparativement à 10% des obs. au hasard, etc.
tab_lift <- hecmulti::courbe_lift(
   prob = cv_prob,
   resp = classif,
   plot = TRUE)
tab_lift

## -----------------------------------------------------------------------------
## Test de calibration de Spiegelhalter
hecmulti::calibration(
  prob = cv_prob,
  resp = classif)


