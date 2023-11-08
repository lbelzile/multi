## -----------------------------------------------------------------------------
library(hecmulti)
library(survival)
data(survie1, package = "hecmulti")
# Stratification par service
 cox_strat <- coxph(
   Surv(temps, censure) ~ age + sexe + strata(service),
   # si on inclut une variable (forcément catégorielle!) dans "strata",
   # cela revient à subdiviser l'échantillon selon la strate
   # et estimer le risque de base séparément pour chaque sous-groupe
   # mais ici les coefficients des variables explicatives sont les mêmes
   # peut importe le service
   data = survie1)
# Décompte par service
with(survie1, table(service, censure))
## # Coefficients
summary(cox_strat)


## -----------------------------------------------------------------------------

# Risque nonproportionnel avec interaction dans le temps
# Il faut OBLIGATOIREMENT modifier la variable à l'intérieur de coxph
cox_np <- coxph(
     Surv(temps, censure) ~
      tt(age) + sexe + service, # la variable modifiée est dans tt()
      data = survie1,
      # définir avec une fonction la nature de l'interaction temporelle
      tt = function(x, t, ...){x + t/52}) 
      
# Le tableau résumé inclura des coefficients et des tests pour les effets      
 summary(cox_np)
# Remarque technique: on ne peut PAS comparer un modèle avec les effets qui dépendent du temps
# à un modèle à risque proportionnel ordinaire en calculant la statistique du rapport de vraisemblance
# à la main...

## -----------------------------------------------------------------------------
# tt() ne gère pas les variables catégorielles, il faut donc créer des variables binaires
# model.matrix crée la matrice du modèle avec les indicateurs

survie1_modif <- survie1 |>
  dplyr:: mutate(service1 = service == 1,
                 service2 = service == 2,
                 service3 = service == 3)
# Avec plusieurs catégories,cela devient fastidieux. 
# Voici une autre méthode plus efficace, mais moins transparente
# Créer la matrice de modèle avec indicateurs binaires pour service
service_bin <- model.matrix(~service, 
                            data = survie1)[,-1]
# enlever ordonnée à l'origine (première colonne de uns)
# Concaténer par colonne pour obtenir une base de données avec toutes les colonnes
survie1_modif <- cbind(survie1, service_bin) 
cox_np <- survival::coxph(
    Surv(temps, censure) ~ 
     age + sexe + service +  # notez que service est AUSSI incluse (effet principal)
      tt(service1) + tt(service2) + tt(service3),  # interactions avec le temps, plusieurs termes
     data = survie1_modif, 
     tt = function(x, t, ...){t * x})

# Tableau résumé - on remarque que deux des trois interactions sont significatives
# et que l'effet n'est sans doute pas le même pour le surenchérissement
# l'effet protecteur diminue
summary(cox_np)

## -----------------------------------------------------------------------------

# Variables explicatives qui changent au fil du temps
# Ici, il faut manipuler la base de donnée pour casser la contribution de chacun
# Chaque ligne correspond à un segment avec les variables fixées
# On utilise la notation de Aalen, avec le début (time) et la fin (time2)
# Précédemment, le temps de début était implicitement 0
data(survie3, package = "hecmulti")
cox4 <- coxph(Surv(time = debut,
                    time2 = fin,
                    event = evenement) ~
                age + sexe + service,
              data = survie3)
# Voir "tmerge" et la vignette de Therneau, Crowson et Atkinson dans le paquet "survival"
# https://cran.r-project.org/web/packages/survival/vignettes/timedep.pdf

## -----------------------------------------------------------------------------

# Modèles à risque compétitif
# Voir vignette https://cran.r-project.org/web/packages/survival/vignettes/compete.pdf
# pour plus de détails techniques

# Rappel pour "event":
#  - 1 (TRUE) pour observation, 
#  - 0 (FALSE) pour censure à droite
data(survie4, package = "hecmulti")
rc_cox_A <- coxph(Surv(time = temps, 
# On peut modifier la valeur de événement, ici l'événement d'intérêt est désabonnement vers A
# Tous les autres types d'événement censure != 2 donnent 0 (censure à droite)
                   event = censure == 2) ~ age + sexe + service, 
              data = survie4)


## -----------------------------------------------------------------------------
# Pour ajuster tous les modèles simultanément, il faut créer une variable
# indicatrice avec l'identifiant du client - ici, valeurs uniques arbitraires pour chacun
n <- nrow(survie4)
surv4 <- survie4 |> 
  dplyr::mutate(id = 1:n)
# On met pour la variable événement un facteur avec catégorie de référence pour l'événement d'intérêt
rc_cox <- coxph(Surv(time = temps, 
             event = factor(censure)) ~ sexe + age + service, # notez le 'factor'
             data = surv4,
             id = id)


## -----------------------------------------------------------------------------
data(survie4, package = "hecmulti")
# Pour le modèle à risques compétitifs, il faut que l'événement soit de type facteur
# Modèle multi-état avec transition possible de abo -> A, abo -> B et abo -> pas de service
# La catégorie de référence est utilisé comme point de départ
rc_km <- survfit(Surv(time = temps, 
             event = factor(censure)) ~ 1, 
             data = survie4)
# Il ne faut SURTOUT pas ajuster plusieurs courbes une à la fois pour Kaplan-Meier
# l'estimation serait faussée


## -----------------------------------------------------------------------------
# Graphiques R de base pour probabilité de chaque état en fonction du temps
par(mfrow = c(1,2), bty = "l")
# mfrow: aligner sur une ligne, deux colonnes
# bty: trace une console en L plutôt qu'une boîte
plot(rc_km, 
     noplot = NULL, # tracer une courbe pour les probabilités de la catégorie de référence
     col = 1:4, # couleurs
     ylab = "probabilité dans l'état") # étiquette de l'axe des x
legend("topright", # positionnement de la légende
       legend = c("abonnement", "A","B","fin"), # étiquettes de la légender
       bty = "n", # ne pas tracer de boîte (contour) autour de la légende
       col = 1:4, 
       lty = 1) # type de ligne
plot(survfit(rc_cox, newdata = hecmulti::survie4[1,]),
     # prédire la survie du premier individu de la base de données
     noplot = NULL,
     col = 1:4)


