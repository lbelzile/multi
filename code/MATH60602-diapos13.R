library(mice)

data(manquantes, package = 'hecmulti')
summary(manquantes)
# Pourcentage de valeurs manquantes
apply(manquantes, 2, function(x){mean(is.na(x))})
# Voir les configurations de valeurs manquantes
md.pattern(manquantes) 

library(mice)
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
