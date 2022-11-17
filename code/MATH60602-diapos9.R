## -----------------------------------------------------------------------------
library(survival)
data(survie1, package = "hecmulti")
# Estimateur de Kaplan-Meier
# La réponse "temps" est le temps de survie
# et l'indicateur de censure "censure" est
# "0" pour censuré à droite, "1" pour événement
kapm <- survfit(
  Surv(temps, censure) ~ 1,
  #aucune variable explicative
  type = "kaplan-meier",
  conf.type = "log", #type d'intervalle de conf.
  data = survie1)


## -----------------------------------------------------------------------------
# Tableau résumé de la survie
summary(kapm)
# Graphique de la fonction de survie
plot(kapm) # graphique de base
# Quantiles (par défaut, quartiles)
quantile(kapm)

# Comparer les courbes de survie selon le sexe
survdiff(formula = Surv(temps, censure) ~ sexe,
         data = survie1)


# -----------------------------------------------------------------------------
cox <- coxph(
  Surv(temps, censure) ~
    age + sexe + region + service,
  data = survie1,
  ties = "exact") # gestion des doublons
# Les autres options, "breslow" et "efron",
# sont moins coûteuses

# Tableau résumé avec coefficients,
# intervalles de confiance de Wald
# et tests pour significativité globale
summary(cox)
# Test du rapport de vraisemblance
car::Anova(cox, type = 3)
# Intervalles de confiance de Wald
confint(cox)

# -----------------------------------------------------------------------------
# Prédictions des courbes de survie
#  avec le modèle de Cox
# Ajuster un modèle avec deux variables
data(survie2, package = "hecmulti")
cox <- coxph(
  Surv(temps, censure) ~
    age + sexe,
  data = survie1)
pred <- survfit(
  cox,       # Modèle de Cox
  newdata = survie2, # nouvelle base de données
  type = "kaplan-meier") # survie
plot(pred) # graphe de base

# Graphique pour le postulat de risque proportionnels

# Graphiques R de base...
# Changer les paramètres graphiques
# mfrow (nb lignes, nb colonnes) pour 2x2
# Rogner les marges avec "mar"
par(mfrow = c(2,2), mar = c(4,4,1,1))
coxzph_out <- cox.zph(cox)

# Graphiques des résidus en fonction du temps
# avec traîne locale
plot(coxzph_out)
# Résultats du test du score
# variable par variable et globalement
print(coxzph_out)

# Fermer la console graphique
graphics.off()
