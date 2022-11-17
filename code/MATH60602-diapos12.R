# PRÉTRAITEMENT DES DONNÉES
# Données de dons
data(dons, package = "hecmulti")
# Données bidons avec regroupements, pour illustration
data(regroupements1, package = "hecmulti")


# Segmentation naturelle des donneurs
# Aucun don / un seul don / dons multiples
donsmult <- dons |>
  dplyr::filter(ndons > 1L) |>
  dplyr::mutate(mtdons = vdons/ndons,
                snrefus = nrefus/anciennete*mean(anciennete),
                mpromesse = dplyr::case_when(
                  npromesse > 0 ~ vpromesse/npromesse,
                  TRUE ~ 0)) |>
  dplyr::select(!c(vradiations, # valeurs manquantes
                   nindecis,
                   vdons,
                   ddonsmax,
                   ddonsmin,
                   vdonsmin,
                   npromesse,
                   vpromesse,
                   nrefus,
                   nradiations)) |>
  dplyr::relocate(mtdons)


#-------------------------------------------------
# STANDARDISATION DES DONNÉES
# Standardisation usuelle
# (soustraire la moyenne, diviser par écart-type)
donsmult_std <- scale(donsmult)
# Extraire moyenne et écart-type
dm_moy <- attr(donsmult_std, "scaled:center")
dm_std <- attr(donsmult_std, "scaled:scale")


##  Regroupements hiérarchiques
# hclust() (paquet "stats") a une implémentation
# avec une complexité typiquement cubique
# malgré un optimum de Omega(n^2)
# flashclust::hclust est plus près de ce dernier

# Stockage de la matrice de distance
flashclust::hclust()

# plus le seuil pour l'index de Gini est élevé, plus
# les petits regroupements sont pénalisés
# entre zéro et 1 (liaison simple)
library(genieclust)
reg_genie <- gclust(d = donsmult_std,
                    gini_threshold = 0.3,
                    distance = "euclidean")
plot(reg_genie,
     labels = FALSE,
     sub = NA,
     xlab = NA,
     ylab = "hauteur",
     main = "Dendrogramme") # dendogramme
genieh_etiquettes <- cutree(reg_genie, k = 5)
table(genieh_etiquettes)
donsmult |>
  group_by(groupe = genieh_etiquettes) |>
  summarise_all(.funs = mean)
# Groupe 1: montant moyen plus élevé, plus récent. Dons total plus élevé, promesses élevées
# Groupe 2: donateurs récents ordinaires
# Groupe 3: donateurs anciens, faibles dons, inactifs, peu de promesses, nombreux refus, délais plus élevé
# Groupe 4: Donneurs intermédiaires (montants moyens plus élevés, valeurs plus élevées, plus de refus)
# Groupe 5: donateurs anciens inactifs, nombre de refus très élevé par rapport à G3, faibles promesses, très petits montants moyens
plot(acp$scores[,1:2],
     col = genieh_etiquettes,
     pch = 20)
