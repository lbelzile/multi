#////////////////////////////////////////////////////#
#  Partie 1 - ajustement d'un modèle de régression   #
#////////////////////////////////////////////////////#
#
# Charger quelques paquets
library(dplyr)
library(hecmulti)
library(tibble)
library(ggplot2)
library(viridis)
# change le thème de base pour les graphiques
theme_set(theme_classic())


remotes::install_github("lbelzile/hecmulti",
                        dependencies = TRUE)
# Données tirées d'une étude sur l'inéquité salariale
# dans un collège américain
data(salaireprof, package = "hecmulti")

# Analyse exploratoire
ggplot(data = salaireprof,
        aes(x = echelon,
            y = salaire)) +
  geom_boxplot() # boîtes à moustaches

ggplot(data = salaireprof,
        mapping = aes(x = service,
                      y = salaire,
                      color = sexe)) +
  geom_point() + # géométrie: nuage de points
  facet_wrap(~echelon) + # un panneau par échelon
  viridis::scale_color_viridis(discrete = TRUE)
# palette de couleur pour daltoniens

# Calculer le salaire moyen par échelon
salaireprof |>
  group_by(echelon) |>
  summarize(salaire_moyen = mean(salaire))

# Modèle avec seulement l'ordonnée à l'origine
# (étalon de mesure) - moyenne
coef(lm(salaire ~ 1, #ordonnée à l'origine
        data = salaireprof))
mean(salaireprof$salaire)

# Modèle avec une seule variable catégorielle
mod <- lm(salaire ~ echelon,
          data = salaireprof)
# Niveau de la variable catégorielle `echelon`
levels(salaireprof$echelon)
# la catégorie de référence est la première valeur
# en ordre alphanumérique, ici `adjoint`
summary(mod)
# Les coefficients représentent
# beta_0: moyenne de la catégorie de référence,
# beta_i: la différence entre les moyenne de catégorie i et celle de la référence
coef(mod)
# Calcul des prédictions du modèle
predict(mod,
        newdata = data.frame(
          echelon = c("adjoint",
                      "aggrege",
                      "titulaire")))
# Les valeurs retournées correspondent à la
# la moyenne de chaque échelon (à arrondi près)

# Calculer l'erreur moyenne quadratique

# résidu = valeurs de la réponse moins prédictions (valeurs ajustées)
resid(mod)
isTRUE(all.equal(salaireprof$salaire - fitted(mod), resid(mod)))
# erreur quadratique moyenne = moyenne du carré des résidus
mean(resid(mod)^2)

# autres méthodes génériques pour modèles linéaires
methods(class = "lm")


#////////////////////////////////////////////////////#
#  Partie 2 - sélection de variables avec polynôme   #
#////////////////////////////////////////////////////#

# Chargement des données
data(polynome, package = "hecmulti")
# Création de vecteurs de taille 10 pour enregistrer les résultats
eqm <- aic <- bic <- eqm_ve <- eqm_vc <- numeric(10)
n <- nrow(polynome)
# Pour créer un échantillon aléatoire (ici 50-50%)
#
# Si on recompile le script, on obtiendra des résultats différents
# puisque notre sélection pour la validation croisée et la validation
# externe est aléatoire

# NOTE: pour piper les dés et obtenir toujours les mêmes nombres aléatoires
# choisissez un entier (n'importe lequel) et appelez la fonction
# `set.seed` pour fixer le germe aléatoire
set.seed(202309)
od <- sample(rep(1:2, length.out = n))

# Estimation de l'erreur quadratique moyenne pour
# les polynômes de degré 1, ..., 10
for(k in seq_len(10)){
  # Estimation du modèle linéaire avec "glm" (comme "lm")
  mod <- glm(y ~ poly(x, degree = k), data = polynome)
  # Erreur quadratique moyenne d'apprentissage
  eqm[k] <- mean(resid(mod)^2)
  # Méthodes de pénalisation - critères d'information
  aic[k] <- AIC(mod)
  bic[k] <- BIC(mod)

  # EQM pour la validation externe
  # ajuster le modèle avec uniquement échantillon d'apprentissage
  mod_appr <- lm(y ~ poly(x, degree = k),
                 data = polynome,
                 subset = od == 1L)
  # prédire les valeurs ajustées pour les X des données de validations
  # mais avec les coefficients du modèle d'apprentissage
  pred <- predict(object = mod_appr,
                  newdata = polynome[od == 2L,])
  # calculer l'erreur quadratique moyenne à la main
  # valeurs de la réponse pour données de validation moins prédictions
  eqm_ve[k] <- mean((polynome[od == 2L,]$y - pred)^2)

  # EQM par validation croisée
  eqm_vc[k] <- boot::cv.glm(data = polynome,
                            glmfit = mod,
                            K = 10)$delta[2]
  # la valeur delta est de longueur 2, le deuxième estimé
  # inclut une correction de biais pour la validation croisée
}

# On pourrait écrire notre propre code pour faire la validation croisée
# Je le fais ci-dessous, uniquement pour vous montrer comment
# les différentes étapes correspondent aux instructions dans le code

n <- nrow(polynome)
nvc <- 10L
polynome <- polynome |>
  mutate(groupe = sample(rep(1:nvc, length.out = n)))
# On peut vérifier qu'on a le bon nombre
(ngp <- table(polynome$groupe))
# polynome |>
#   group_by(groupe) |>
#   summarize(decompte = n())

somme_err_quad <- matrix(nrow = nvc, ncol = 10)
# pour chaque pli
for(i in seq_len(nvc)){
  # pour chaque degré de polynôme
  for(j in seq_len(10)){
    # Ajuster modèle sur toutes les données, moins le ie pli
  mod <- lm(y ~ poly(x = x, degree = j),
            data = polynome,
            subset = groupe != i)
  pred <- predict(mod, newdata = polynome |> filter(groupe == i))
  reponse <- polynome |> filter(groupe == i) |> select(y) |> unlist()
  somme_err_quad[i,j] <- sum((reponse - pred)^2)
 }
}
# Additioner les erreurs de chaque pli, puis diviser par taille totale
# (cela revient à calculer la moyenne)
eqm_poly <- colSums(somme_err_quad) / n
# Calculer l'écart-type des valeurs de l'erreur quadratique moyenne de chaque pli
erreur_type_eqm <- numeric(length = ncol(somme_err_quad))
for(j in seq_along(erreur_type_eqm)){
  erreur_type_eqm[j] <- sd(somme_err_quad[,j]/ngp[j])
}

# Tracer un graphique de la performance
ggplot(data = data.frame(k = 1:10,
                         eqm = eqm, # erreur quadratique moyenne (sans ajustement)
                         eqm_ve = eqm_ve, # validation externe
                         eqm_vc = eqm_poly, # validation croisée (vc)
                         et_vc = erreur_type_eqm), # estimation de l'erreur-type de la vc
       ) +
  # ajouter points pour chaque valeur de k
  geom_point(mapping = aes(x = k, y = eqm)) +
  geom_point(mapping = aes(x = k, y = eqm_ve),
            color = "green") +
  geom_pointrange(mapping = aes(x = k, y = eqm_vc,
                                ymax = eqm_vc + et_vc,
                                ymin = eqm_vc),
            color = "violet",
            linetype = "dashed") +
  scale_x_continuous(breaks = 1:10)
# Notez l'énorme incertitude pour la validation croisée avec
#  une si petite taille d'échantillon
# Note: On pourrait aussi obtenir une estimation de l'erreur-type
# de la validation croisée en répliquant cette dernière 10 ou 20 fois
# retourner la moyenne comme estimateur, et calculer l'erreur-type
# à partir de l'écart-type des différents estimés retournés pour chaque réplication
