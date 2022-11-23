## Regroupements hiérarchiques
# "fastClust", "genieclust"
## Mélanges de modèles
# "mclust", "mixture"
## Regroupements basés sur la densité
# "dbscan"


# PRÉTRAITEMENT DES DONNÉES
# Données de dons
data(dons, package = "hecmulti")

#-------------------------------------------------
# TRANSFORMATION DES DONNÉES
# Segmentation naturelle des donneurs
# Aucun don / un seul don / dons multiples
donsmult <- dons |>
  dplyr::filter(ndons > 1L) |>
  dplyr::mutate(
    mtdons = vdons/ndons, #montant moyen des dons
    snrefus = nrefus/anciennete*mean(anciennete),
    # refus relatif à l'ancienneté
    mpromesse = dplyr::case_when(
      npromesse > 0 ~ vpromesse/npromesse,
      TRUE ~ 0)) |> # montant moyen des promesses
  dplyr::select(!c(vradiations, # valeurs manquantes
   # Enlever autres variables pas pertinentes
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
#mettre comme première colonne


#-------------------------------------------------
# STANDARDISATION DES DONNÉES
# Standardisation usuelle
# (soustraire la moyenne, diviser par écart-type)
donsmult_std <- scale(donsmult)
# Extraire moyenne et écart-type
dm_moy <- attr(donsmult_std, "scaled:center")
dm_std <- attr(donsmult_std, "scaled:scale")

#------------------------------
# K-MÉDOÏDES: PAM & CLARA
kmedoide <- list()
set.seed(60602)
ngmax <- 10L # nb max de regroupements
for(k in seq_len(ngmax)){
  # Algorithme quadratique en `sampsize`
  kmedoide[[k]] <- cluster::clara(
    x = donsmult_std, # données
    k = k, #nb regroupements
    sampsize = 500, # taille du sous-échantillon (max ~ 1000 obs)
    # la qualité dépend de sampsize (comme le coût de calcul!)
    metric = "euclidean", # mesure de dissemblance
    #cluster.only = TRUE, # option pour ne conserver que étiquettes
    rngR = TRUE, # germe aléatoire depuis R
    pamLike = TRUE, # même algorithme que PAM
    samples = 10) # nombre de répétitions - seule la meilleur segmentation est retournée
}
# Regarder utilitaires pour une méthode
# Graphique des silhouettes pour K=4
plot(factoextra::fviz_silhouette(kmedoide[[4]]))
# Obtenir les prototypes (les médoïdes sont des observations), mais remettre à l'échelle originale
donsmult[kmedoide[[4]]$i.med,]
# Taille des regroupements
kmedoide[[4]]$clusinfo

#-------------------------------------------------
# DBSCAN ET OPTICS

# M=5 observations par groupe par défaut
library(dbscan)
# Distance des K-plus proches voisins (M-1 ici)
kNNdistplot(donsmult_std, minPts = 10)
# On voit eps=2 dans le graphique pour le coude
db <- dbscan::dbscan(
  x = donsmult_std,
  eps = 2,
  minPts = 10)
# Ici, segmentation pas intéressante du tout
# En cause, il y a un continuum et
# aucune rupture dans les montants, durées, etc.
# qui permette de séparer les groupes
db
# Étiquettes (0 pour bruit/aberrance)
#db$cluster

#-------------------------------------------------
# MÉLANGES DE MODÈLES GAUSSIENS
set.seed(60602)
# Paquet mclust
library(mclust)
mmg <- Mclust(data = donsmult_std,
       G = 2:10, #nombre de regroupements
       # Ajouter composante uniforme
       #  pour bruit (aberrances)
       initialization = list(noise = TRUE))
# Résumé de la segmentation
summary(mmg)
# Étiquettes (avec 0 pour bruit)
mmg$classification
# Graphique de -BIC
plot(mmg, what = "BIC")
# Nombre final de groupe choisi par BIC
mmg$G
# Paramètres
mmg$parameters$pro #prob de chaque composante

# moyenne des segments
mmg$parameters$mean*dm_std + dm_moy
# (un groupe par colonne)
# ATTENTION: ces paramètres ne correspondent pas
# nécessairement aux estimés empiriques
# des moyennes desregroupements

# Probabilité de chaque observation dans chaque groupe (n par K)
# mmg$z

# Matrice des nuage de points (paires de variables)
# plot(mmg, what = "classification")
reduc_dim_mmg <- MclustDR(mmg)
# Diagramme avec réduction de la dimension
par(mfrow = c(1,2))
# Contour des ellipsoïdes sur projection
plot(reduc_dim_mmg, what = "contour")
# Message d'erreur à cause de la composante uniforme
plot(reduc_dim_mmg, what = "scatterplot")

#-------------------------------------------------
##  REGROUPEMENTS HIÉRARCHIQUES
# hclust() (paquet "stats") a une implémentation
# avec une complexité typiquement cubique
# malgré un optimum de Omega(n^2)
# flashclust::hclust est plus près de ce dernier

# Regroupements hiérarchiques agglomératifs avec modèles gaussiens
# mclust::hc()
# Modèles agglomératifs avec distance d'énergie (fonction caractéristique pondérée)
# energy::energy.hclust()


# Attention, cette opération prend 750MB
dist_euc <- dist(donsmult_std, method = "euclidean")

# Stockage de la matrice de distance
reghier_ward <- flashClust::hclust(
  d = dist_euc,
  method = "ward")
# Dendrogramme
plot(reghier_ward)
rect.hclust(reghier_ward, k = 8)
# R-carré et R-carré semi-partiel
hecmulti::homogeneite(
    rhier = reghier_ward,
    data = donsmult_std,
    ngroupes = 15L)

reghier_simple <- flashClust::hclust(
  d = dist_euc,
  method = "single")

reghier_complet <- flashClust::hclust(
  d = dist_euc,
  method = "complete")

reghier_energie <- energy::energy.hclust(
  dst = dist_euc)

# Procédure agglomérative avec
# liaison simple modifiée (GENIE)
# plus le seuil pour l'index de Gini
#  (`gini_threshold`) est élevé, plus
# les petits regroupements sont pénalisés
# valeur comprise entre 0 et 1 (1=liaison simple)
library(genieclust)
reg_genie <- gclust(d = donsmult_std,
                    gini_threshold = 0.3,
                    distance = "euclidean")
# dendogramme
plot(reg_genie,
     labels = FALSE,
     sub = NA,
     xlab = NA,
     ylab = "hauteur",
     main = "Dendrogramme")
# Conserver 5 regroupements
# Élagage avec cutree
genieh_etiquettes <- cutree(reg_genie, k = 5)
# Décompte par regroupement
table(genieh_etiquettes)
# Statistiques descriptives au sein des regroupements
donsmult |>
  group_by(groupe = genieh_etiquettes) |>
  summarise_all(.funs = mean)

# Interprétation des regroupements
# Groupe 1: montant moyen plus élevé, plus récent. Dons total plus élevé, promesses élevées
# Groupe 2: donateurs récents ordinaires
# Groupe 3: donateurs anciens, faibles dons, inactifs, peu de promesses, nombreux refus, délais plus élevé
# Groupe 4: Donneurs intermédiaires (montants moyens plus élevés, valeurs plus élevées, plus de refus)
# Groupe 5: donateurs anciens inactifs, nombre de refus très élevé par rapport à G3, faibles promesses, très petits montants moyens


# Représenter les groupes avec les composantes principales
library(ggplot2)
acp <- princomp(donsmult_std)
ggplot(
  data = data.frame(
    pc1 = acp$scores[,1],
    pc2 = acp$scores[,2],
    etiquettes = genih_etiquettes),
  mapping = aes(x = pc1,
                y = pc2,
                col = etiquettes,
                pointype = etiquettes)) +
  geom_point(alpha = 0.1)