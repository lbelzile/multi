data(polynome, package = "hecmulti")
eqm <- aic <- bic <- eqm_ve <- eqm_vc <- rep(0, 10)
n <- nrow(polynome)
# Pour créer un échantillon aléatoire (ici 50-50%)
od <- sample.int(n, n)
# Sélectionner avec filter selon la condition logique
polynome_appre <- polynome |> dplyr::filter(od <= n/2)
# Autre façon de sélectionner les lignes
polynome_valid <- polynome[od > n/2,]

# Estimation de l'erreur quadratique moyenne pour 
# les polynômes de degré 1, ..., 10
for(k in 1:10){
  # Estimation du modèle linéaire avec "glm" (comme "lm")
  mod <- glm(y ~ poly(x, degree = k), data = polynome)
  # Erreur quadratique moyenne d'apprentissage
  eqm[k] <- mean(resid(mod)^2)
  # Méthodes de pénalisation - critères d'information
  aic[k] <- AIC(mod)
  bic[k] <- BIC(mod)
 
  # EQM pour la validation externe
  mod_appr <- lm(y ~ poly(x, degree = k),
                 data = polynome_appre)
  pred <- predict(object = mod_appr,
                  newdata = polynome_valid)
  eqm_ve[k] <- mean((polynome_valid$y - pred)^2)
 
  # EQM par validation croisée
  eqm_vc[k] <- boot::cv.glm(data = polynome,
                            glmfit = mod,
                            K = 10)$delta[2]
}

# Tracer un graphique de la performance
library(ggplot2)
ggplot(data = data.frame(k = 1:10,
                         eqm = eqm,
                         eqm_ve = eqm_ve)) +
  geom_line(mapping = aes(x = k, y = eqm)) +
  geom_line(mapping = aes(x = k, y = eqm_ve),
            color = "green") +
  geom_line(mapping = aes(x = k, y = eqm_vc),
            color = "violet",
            linetype = "dashed") +
  scale_x_continuous(breaks = 1:10) +
  theme_classic()
  
# Si on recompile le script, on obtiendra des résultats différents
# puisque notre sélection pour la validation croisée et la validation
# externe est aléatoire
