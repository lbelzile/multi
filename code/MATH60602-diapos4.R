## ---------------------------------------------------------------------------------
data(dbm, package = "hecmulti")
str(dbm)


## ---------------------------------------------------------------------------------
dbm_sub <- dbm |>
  dplyr::filter(test == 0) |>
  dplyr::select(!test)

table(dbm_sub$x1)
table(dbm_sub$x5)
table(dbm_sub$x3)
table(dbm_sub$x4)



## ---------------------------------------------------------------------------------
library(ggplot2)
library(patchwork)
g1 <- ggplot(data = dbm_sub,
             aes(x = x2)) +
      geom_histogram(alpha = 0.5,
                     aes(y = ..density..),
                     bins = 30) +
  labs(y = "densité",
       x = "âge")
g2 <- ggplot(data = dbm_sub,
             aes(
                 x = x8
                 )) +
      geom_histogram(alpha = 0.5,
                     aes(y = ..density..),
                     bins = 30) +
  labs(y = "densité",
       x = "montant du dernier achat (en dollar)")
g3 <- ggplot(data = dbm_sub,
             aes(x = x9)) +
      geom_histogram(alpha = 0.5,
                     aes(y = ..density..),
                     bins = 30) +
  labs(y = "densité",
       x = "montant total dépensé depuis un an (en dollars)")
g4 <- ggplot(data = dbm_sub,
             aes(x = x7 )) +
      geom_histogram(alpha = 0.5,
                     aes(y = ..density..),
                     bins = 30) +
  labs(y = "densité",
       x = "nombre de semaines depuis le dernier achat")
g5 <- ggplot(data = dbm_sub,
             aes(x = x6)) +
      geom_histogram(alpha = 0.5,
                     aes(y = ..density..),
                     bins = 30) +
  labs(y = "densité",
       x = "ancienneté comme client")
g6 <- ggplot(data = dbm_sub,
             aes(x = x10
                 )) +
      geom_histogram(alpha = 0.5,
                     aes(y = ..density..),
                     bins = 30) +
  labs( y = "densité",
       x = "nombre d'achats différents depuis un an")
library(patchwork)
(g1 + g2)/(g3 + g4) / (g5 + g6) +
  patchwork::plot_layout(guides = 'collect') &
  theme_classic() &
  theme(legend.position = "bottom")



## ---------------------------------------------------------------------------------
dbm_sub_c <- dbm_sub |>
    dplyr::select(x2, x6, x7, x8, x9, x10)
tibble::tibble(variable = c("x2","x6","x7","x8","x9","x10"),
                               moyenne = apply(dbm_sub_c, 2, mean),
               "écart-type" = apply(dbm_sub_c, 2, sd),
               min = apply(dbm_sub_c, 2, min),
               max = apply(dbm_sub_c, 2, max)) |>
  knitr::kable(digits = 2, booktabs = TRUE, linesep = "") |>
  kableExtra::kable_styling(full_width = TRUE)

tibble::tibble(variable = c("x2","x6","x7","x8","x9","x10"),
               description = c("âge",
                               "nombre d’année comme client",
                               "nombre de semaines depuis le dernier achat",
                               "montant du dernier achat",
                               "montant total dépensé sur un an",
                               "nombre d'achats différents sur un an")) |>
  knitr::kable(booktabs = TRUE, linesep = "") |>
  kableExtra::kable_styling(full_width = TRUE)



## ---------------------------------------------------------------------------------
data(dbm, package = "hecmulti")
dbm_a <- dbm |>
  dplyr::filter(
    test == 0, #données d'entraînement
    !is.na(ymontant)) # personnes qui ont acheté


## ---------------------------------------------------------------------------------
# (...)^2 crée toutes les interactions d'ordre deux
# I(x^2) permet de créer les termes quadratiques
formule <-
  formula(ymontant ~
          (x1 + x2 + x3 + x4 + x5 +
             x6 + x7 + x8 + x9 + x10)^2 +
            I(x2^2) + I(x6^2) + I(x7^2) +
            I(x8^2) + I(x9^2) + I(x10^2))
mod_complet <- lm(formule, data = dbm_a)
# Matrice avec toutes les variables
matmod <- model.matrix(mod_complet)


## ---------------------------------------------------------------------------------
# Recherche exhaustive avec variables de base
rec_ex <- leaps::regsubsets(
  x = ymontant ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10,
  nvmax = 13L,
  method = "exhaustive",
  data = dbm_a)
resume_rec_ex <- summary(rec_ex,
                         matrix.logical = TRUE)
# Trouver le modèle avec le plus petit BIC
min_BIC <- which.min(resume_rec_ex$bic)
# Nom des variables dans le modèle retenu
rec_ex$xnames[resume_rec_ex$which[min_BIC,]]




## ---------------------------------------------------------------------------------
seq_AIC <- MASS::stepAIC(
  lm(ymontant ~ 1, data = dbm_a),
  # modèle initial sans variables explicative
    scope = formule, # modèle maximal possible
    direction = "both", #séquentielle
    trace = FALSE, # ne pas imprimer le suivi
    keep = function(mod, AIC, ...){
      # autres sorties des modèles à conserver
      list(bic = BIC(mod),
           coef = coef(mod))},
    k = 2) #
# Remplacer k=2 par k = log(nrow(dbm_a)) pour BIC


## ---------------------------------------------------------------------------------
library(glmnet)
lambda_seq <- seq(from = 0.01, to = 2, by = 0.01)
cv_output <-
  glmnet::cv.glmnet(x = as.matrix(matmod),
            y = dbm_a$ymontant,
            alpha = 1,
            lambda = lambda_seq)
plot(cv_output)


## ---------------------------------------------------------------------------------

# Ajuster le modèle plusieurs fois et tracer un diagramme
# avec les valeurs des coefficients
lasso_path <-
  glmnet::glmnet(
    x = as.matrix(as.matrix(matmod)),
    y = dbm_a$ymontant,
    alpha = 1,
    lambda = seq(from = 0.01, to = 10, by = 0.01))
plot(lasso_path)


## ---------------------------------------------------------------------------------
lambopt <- cv_output$lambda.min #ou cv_output$lambda.1se
lasso_best <-
  glmnet::glmnet(
    x = as.matrix(as.matrix(matmod)),
    y = dbm_a$ymontant,
    alpha = 1,
    lambda = lambopt)


## ---------------------------------------------------------------------------------
# Prédictions et calcul de l'EQM
# Données externes
dbm_v <- dbm |>
  dplyr::filter(
    test == 1,
    !is.na(ymontant))
pred <- predict(lasso_best,
                s = lambopt,
                newx = as.matrix(
                  model.matrix(formule,
                               data = dbm_v)))
eqm_lasso <- mean((pred - dbm_v$ymontant)^2)

