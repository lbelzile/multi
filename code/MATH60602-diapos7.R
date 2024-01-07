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
apply(manquantes, 2, function(x) {
  mean(is.na(x))
})
# Voir les configurations de valeurs manquantes
md.pattern(manquantes)

# Intensif en calcul, réduire "m" si nécessaire
impdata <- mice(
  data = manquantes,
  # argument method pour le type de modèles
  # selon les variables
  m = 50,
  # nombre d'imputations
  seed = 60602,
  # germe aléatoire
  printFlag = FALSE
) # ne pas imprimer le suivi
# Chaque copie est disponible (1, ..., 50)
complete(impdata, action = 1)
# ajuste le modèle avec les données imputées
adj_im <- with(data = impdata,
               expr = glm(y ~ x1 + x2 + x3 + x4 + x5 + x6,
                          family = binomial))
# combinaison des résultats
fit <- pool(adj_im)
# Tableau résumé avec valeurs-p, degrés de liberté et erreur-types corrigées
summary(fit)

## -----------------------------------------------------------------------------
data(vote, package = "hecmulti")
levels(vote$catvote)
# Modèle multinomial
multi1 <- nnet::multinom(
  catvote ~ age + sexe + race + revenu + educ + affiliation,
  data = vote,
  # base de données
  subset = age > 30,
  # sous-ensemble
  weights = poids,
  # poids de sondage
  trace = FALSE
)     # infos sur convergence


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
predict(multi1, type = "class")

# Modèle avec uniquement l'ordonnée à l'origine (~ 1)
# Il faut qu'on utilise les mêmes données pour la comparaison!
multi0 <- nnet::multinom(
  catvote ~ 1,
  weights = poids,
  data = vote,
  subset = age > 30,
  trace = FALSE
)
# Test de rapport de vraisemblance
anova(multi0, multi1)

# Modèle logistique à cote proportionnelle
with(vote, is.ordered(catvote))
multi2a <- MASS::polr(
  catvote ~ sexe,
  # formule
  data = vote,
  subset = age > 30,
  weights = poids,
  method = "logistic",
  Hess = TRUE
) # Calculer la hessienne (pour erreurs-types)
# message d'avertissement en raison de poids non-entiers
# (vous pouvez les ignorer sans conséquence)

summary(multi2a)

# IC pour beta_x (vraisemblance profilée)
confint(multi2a)


# Critères d'information
AIC(multi2a)
BIC(multi2a)
# Tableau des coefficients
# Coefficients (variables explicatives)
coef(multi2a)
# Ordonnées à l'origine:
multi2a$zeta

# Modèle de régression logistique multinomiale, avec la même formule
multi2b <- nnet::multinom(
  catvote ~ sexe,
  data = vote,
  subset = age > 30,
  weights = poids,
  trace = FALSE
)

# Combien de paramètres de plus avec modèle logistique?
# Même nombre d'ordonnées à l'origine, mais pour les p variables explicatives
# (K-1)*p vs p  paramètres, une différence de (K-2) * p
difddl <- (length(multi2a$zeta) - 1) * length(coef(multi2a))
# Valeur-p du test de rapport de vraisemblance
# Probabilité qu'une variable khi-deux avec ddl(0)-ddl(1)
#  dépasse la valeur numérique de la statistique
pchisq(
  q = deviance(multi2a) - deviance(multi2b),
  df = difddl,
  lower.tail = FALSE
)


# Nouveau modèle, cette fois avec la variable âge
# On recentre la variable explicative pour faciliter l'optimisation

multi3a <- MASS::polr(
  catvote ~ scale(age, scale = FALSE),
  data = vote,
  subset = age > 30,
  weights = poids,
  method = "logistic",
  Hess = TRUE
)

multi3b <- nnet::multinom(
  catvote ~ scale(age, scale = FALSE),
  data = vote,
  subset = age > 30,
  weights = poids,
  Hess = TRUE,
  trace = FALSE
)
# Valeur-p du test de rapport de vraisemblance
pval <- pchisq(
  q = deviance(multi3a) - deviance(multi3b),
  df = length(coef(multi2a)),
  lower.tail = FALSE
)
# calcul des prédictions à l'échelle des probabilités pour chaque modèle
xpred <- seq(30, 95, by = 0.1) - mean(vote$age)
nobs <- length(xpred)
pred1 <- predict(multi3b,
                 newdata = data.frame(age = xpred),
                 type = "prob")
pred2 <- predict(multi3a,
                 newdata = data.frame(age = xpred),
                 type = "prob")


# Graphiques dans les diapositives
#
library(ggplot2)
library(dplyr)
library(patchwork)
data(vote, package = "hecmulti")
g1 <- vote |>
  dplyr::count(revenu, catvote, wt = poids) |>
  ggplot(aes(fill = catvote,
             x = n,
             y = revenu)) +
  ggplot2::geom_bar(position = "fill",
                    stat = "identity") +
  scale_x_continuous(
    name = NULL,
    position = "top",
    expand = c(0, 0),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0%", "25", "50", "75", "100")
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    subtitle = "revenu",
    x = "",
    y = "",
    fill = "vote"
  ) +
  theme_classic()
g2 <- vote |>
  dplyr::count(sexe, catvote, wt = poids) |>
  ggplot(aes(fill = catvote,
             x = n,
             y = sexe)) +
  ggplot2::geom_bar(position = "fill",
                    stat = "identity") +
  scale_x_continuous(
    name = NULL,
    position = "top",
    expand = c(0, 0),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0%", "25", "50", "75", "100")
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    subtitle = "sexe",
    x = "",
    y = "",
    fill = "vote"
  ) +
  theme_classic()
g3 <- vote |>
  dplyr::count(race, catvote, wt = poids) |>
  ggplot(aes(fill = catvote,
             x = n,
             y = race)) +
  ggplot2::geom_bar(position = "fill",
                    stat = "identity") +
  scale_x_continuous(
    name = NULL,
    position = "top",
    expand = c(0, 0),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0%", "25", "50", "75", "100")
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    subtitle = "race",
    x = "",
    y = "",
    fill = "vote"
  ) +
  theme_classic()
g4 <- vote |>
  dplyr::count(educ, catvote, wt = poids) |>
  ggplot(aes(fill = catvote,
             x = n,
             y = educ)) +
  ggplot2::geom_bar(position = "fill",
                    stat = "identity") +
  scale_x_continuous(
    name = NULL,
    position = "top",
    expand = c(0, 0),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0%", "25", "50", "75", "100")
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    subtitle = "niveau d'éducation",
    x = "",
    y = "",
    fill = "vote"
  ) +
  theme_classic()
g5 <- vote |>
  dplyr::count(affiliation, catvote, wt = poids) |>
  ggplot(aes(fill = catvote,
             x = n,
             y = affiliation)) +
  ggplot2::geom_bar(position = "fill",
                    stat = "identity") +
  scale_x_continuous(
    name = NULL,
    position = "top",
    expand = c(0, 0),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0%", "25", "50", "75", "100")
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    subtitle = "affiliation politique",
    x = "",
    y = "",
    fill = "vote"
  ) +
  theme_classic()
g6 <- vote |>
  mutate(agecat = with(vote, cut(age, c(20, 40, 60,
                                        100)))) |>
  dplyr::count(agecat, catvote, wt = poids) |>
  ggplot(aes(fill = catvote,
             x = n,
             y = agecat)) +
  ggplot2::geom_bar(position = "fill",
                    stat = "identity") +
  scale_x_continuous(
    name = NULL,
    position = "top",
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0%", "25", "50", "75", "100"),
    expand = c(0, 0)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    subtitle = "âge",
    x = "",
    y = "",
    fill = "vote"
  ) +
  theme_classic()
# Combinaison de graphiques avec "patchwork"
(g1 + g2) / (g3 + g4) / (g5 + g6) + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


## Graphiques supplémentaires tirés des diapositives
## Fourni à des fins de reproductibilité seulement.
# Ajustement d'un modèle multinomial avec lissage local
# via modèle généralisé additif (utilisé ici à des fins exploratoires)
votecat <- as.integer(factor(vote$catvote, ordered = FALSE)) - 1L
mod <- mgcv::gam(
  formula = list(votecat ~ s(age, bs = "cr"),
                 ~ s(age, bs = "cr")),
  weights = poids,
  family = mgcv::multinom(K = 2),
  data = vote
)

newage <- 20:90
nb <- length(newage)
pred <- c(predict(mod,
                  newdata = data.frame(age = newage),
                  type = "response"))
cat <-
  factor(rep(c(
    "rarement/jamais", "occasionnellement", "toujours"
  ), each = nb))
cat <- relevel(cat, ref = "rarement/jamais")
ggplot(
  data = data.frame(
    age = rep(newage, 3L * nb),
    pred = pred,
    cat = cat
  ),
  mapping = aes(
    x = age,
    y = pred,
    col = cat,
    group = cat
  )
) +
  geom_line() +
  scale_y_continuous(
    limits = c(0, 1),
    expand = c(0, 0),
    labels = scales::percent
  ) +
  labs(
    color = "catégorie de vote",
    x = "âge)",
    y = "",
    subtitle = "Proportion par catégorie de vote (lissage)"
  ) +
  viridis::scale_color_viridis(discrete = TRUE) +
  theme_classic() +
  theme(legend.position = "bottom")

ggplot(data = vote,
       aes(x = age, fill = catvote)) +
  geom_density(alpha = 0.2) +
  labs(fill = "vote",
       x = "âge (années)",
       y = "densité") +
  theme(legend.position = "bottom") +
  theme_minimal()
