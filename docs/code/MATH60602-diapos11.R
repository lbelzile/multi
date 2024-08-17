library(cluster)
library(flexclust)
library(factoextra)
library(ggplot2)
library(patchwork)
library(hecmulti)
## Regroupements basés sur centroïdes, etc.
# "cluster", "flexclust"

#-------------------------------------------------
# PRÉTRAITEMENT DES DONNÉES
# Données de dons
data(dons, package = "hecmulti")
# Données bidons avec regroupements, pour illustration
data(regroupements1, package = "hecmulti")

dons |>
  filter(ndons >= 1) |>
  str()
  summarize_all(.funs = mean, na.rm = TRUE)

# Segmentation naturelle des donneurs
# Aucun don / un seul don / dons multiples
donsmult <- dons |>
  dplyr::filter(ndons > 1L) |>
  dplyr::mutate(
    mtdons = vdons/ndons,
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

corrplot::corrplot(cor(donsmult))
# ATTENTION À DÉCLARER LES VARIABLES CATÉGORIELLE
# (y compris binaires) selon le métrique


#-------------------------------------------------
# STANDARDISATION DES DONNÉES
# Standardisation usuelle
# (soustraire la moyenne, diviser par écart-type)
donsmult_std <- scale(donsmult)
# Extraire moyenne et écart-type
dm_moy <- attr(donsmult_std, "scaled:center")
dm_std <- attr(donsmult_std, "scaled:scale")
# Standardisation robuste
donsmult_std_rob <- apply(
  donsmult,
  MARGIN = 2,
  FUN = function(x){(x - median(x))/mad(x)})
# FUN = function(x){(x - median(x))/range(x)})
# apply permet d'appliquer une fonction
# par ligne, colonne ou cellule
# MARGIN = 2 indique colonne
# (on centre chaque colonne tour à tour)



#-------------------------------------------------
# MESURES DE DISSEMBLANCE ET DE SIMILARITE
# Distance Euclidienne, de Manhattan, de Gower
d1 <- dist(regroupements1, method = "euclidean")
d2 <- dist(regroupements1, method = "minkowski", p = 1)
d3 <- cluster::daisy(regroupements1, metric = "gower")
# Voir aussi ?flexclust::dist2

#-------------------------------------------------
# ALGORITHMES POUR LA SEGMENTATION
### Algorithme 1: K-moyennes

# Estimer le modèle en faisant varier le nombre de
# regroupements
# Valeurs de départ aléatoires (10 répétitions)
set.seed(60602)
kmoy <- list()
ngmax <- 10L
for(i in seq_len(ngmax)){
 kmoy[[i]] <- kmeans(donsmult_std,
                     centers = i, 
                     #centers: nb de regroupements ou valeurs initiales
                     nstart = 10)
                      #nstart: nb de répétitions aléatoires (seule la meilleure solution est retournée)
}

kmoy5 <- kmoy[[5]]
kmoy5$size # nombre d'observations par groupe
kmoy5$withinss # valeur du critère (fonction objective)
t(t(kmoy5$centers)*dm_std + dm_moy)
# Déterminer le nombre de groupes avec critères

# Somme carré intra-groupes et somme carré totale
scd <- sapply(kmoy, function(x){x$tot.withinss})

# Homogénéité et pourcentage de variance expliquée
# Graphique du R-carré et du R-carré semi-partiel
homogene <- hecmulti::homogeneite(scd, which = 1)
bic_kmoy <- sapply(kmoy, BIC)
ggplot(data = data.frame(bic = bic_kmoy,
                         ng = seq_along(kmoy)),
       mapping = aes(x = ng, y = bic)) +
  geom_line() +
  scale_x_continuous(breaks = seq_along(kmoy)) +
  labs(x = "nombre de regroupements",
       y = "",
       subtitle = "Critère BIC (somme du carré des distances)") +
  theme_classic()



# Silhouette: objectif, maximiser la silhouette moyenne

# Le calcul et le stockage de la matrice
# de distance est trop coûteux ici
# on calcule un ersatz à partir d'un échantillon
# de 1000 observations
# comme la validation croisée, cette mesure sera
# aléatoire!
sub <- sample.int(
  n = nrow(donsmult),
  size = 1000)
silhouettes_kmoy <- rep(0, ngmax-1)
dist_sub <- cluster::daisy(
  x = donsmult_std[sub,],
  metric = "euclidean")
for(i in seq_len(ngmax-1L)){
  silhouettes_kmoy[i] <- summary(
    cluster::silhouette(
      x = kmoy[[i+1]]$cluster[sub],
      dist = dist_sub))$avg.width
}
# Classement (du plus grand au plus petit)
order(silhouettes_kmoy, decreasing = TRUE) + 1
# Suggère 8 regroupements
# Visualisation des silhouettes (ici avec 5 regroupements)
factoextra::fviz_silhouette(
  cluster::silhouette(
    x = kmoy[[8]]$cluster[sub],
    dist = dist_sub))
# Statistique de la brèche
# ATTENTION aussi intensif en calcul
gap <- cluster::clusGap(donsmult_std[sub,],
                        FUN = kmeans,
                        nstart = 20,
                        K.max = 10,
                        B = 60)
plot(gap)
# La règle du premier minimum à un écart-type donne 10 groupes
print(gap)
# En raison de la très grande taille d'échantillon,
# k-moyennes avec un grand nombre de groupes est préféré



# Attention: le résultat des k-moyennes
# est aléatoire (parce que les valeurs
# initiales des prototypes le sont) et
# les étiquettes peuvent être permutées
# même si les regroupements sont les mêmes
kmoy5 <- kmoy[[5]]
# Regarder la répartition
kmoy5$size
# Visualiser les regroupements en projetant
# sur les composantes principales
acp <- princomp(donsmult_std)
pairs(x = acp$scores[,1:3],	
      lower.panel = NULL,
      col = kmoy5$cluster)
# Les coordonnées des prototypes
# (mais données standardisées = pas interprétable...)
kmoy5$centers
# Statistiques descriptives des profils
donsmult |>
  group_by(groupe = kmoy5$cluster) |>
  summarise_all(mean)

# Les regroupements sont interprétables:
# Groupe 1: Petits donateurs, faible nombre de dons. N'ont pas donné depuis longtemps. Refus fréquents et délai entre dons élevés
# Groupe 2: Plus petit groupe. Grands donateurs fidèles: plusieurs dons, valeur maximale élevée. N'ont pas donné récemment
# Groupe 3: Donateurs récents, dons plus élevés que la moyenne mais beaucoup de dons dons de faible valeur, dons peu fréquents. Récidivistes, mais petits donateurs
# Groupe 4: Petits nouveaux. Moins d'ancienneté, dons fréquents et refus fréquents relativement à l'ancienneté.
# Groupe 5: Petits donateurs inactifs. Plutôt anciens, plusieurs refus


# Modifier la matrice de distance et l'initialisation
# distance de Manhattan (k-medianes) et k-moyennes++
set.seed(60602)
kmed5 <- flexclust::kcca(
  x = donsmult_std,
  k = 5,
  family = kccaFamily("kmedians"),
  control = list(initcent = "kmeanspp"))
# Vérifier répartition
kmed5@clusinfo
# Obtenir étiquettes
kmed5@cluster
# Coordonnées des K-médianes (standardisées)
t(t(kmed5@centers)*dm_std + dm_moy)
# G1: Réguliers: petits dons, ancienneté
# G2: nouveaux donateurs, dons récents, pas de refus
# G3: nouveaux donateurs,  dons récents, refus fréquents pour durée
# G4: petits inactifs, plusieurs refus
# G5: riches donateurs

# Indice Rand
randIndex(kmoy5$cluster, kmed5@cluster)

