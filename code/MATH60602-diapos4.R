
# Vérifier si les paquets nécessaires sont installés, les installer au besoin
paquets <- c("dplyr", "ggplot2", "patchwork", "knitr", "remotes", "MASS", "leaps", "glmnet")
for(paquet in paquets){
   if(!paquet %in% rownames(installed.packages())){
      install.packages(paquet)
   }
}
# Charger les paquets
library(ggplot2)
library(patchwork)
# (Ré)-installer la base de données si elle n'a pas été modifié
remotes::install_github("lbelzile/hecmulti")

# Charger les données
data(dbm, package = "hecmulti")
# Imprimer une description succincte
str(dbm)

# Sélection des 1000 données tests
dbm_sub <- dbm |>
  dplyr::filter(test == 0) |>
  dplyr::select(!test) # enlever colonne superflue
# Tableau de contingence 
# Décompte des modalités des variables catégorielles
table(dbm_sub$x1)
table(dbm_sub$x5)
table(dbm_sub$x3)
table(dbm_sub$x4)

# Analyse exploratoire des données

# Histogramme de l'âge
g1 <- ggplot(data = dbm_sub,
             aes(x = x2)) +
      geom_histogram( # histogramme
         alpha = 0.5, # transparence à 50%
         aes(y = ..density..), # mettre à l'échelle de la densité
         bins = 30) + # contrôler le nombre de groupes
  labs(y = "densité", # libellés des étiquettes pour l'axe des ordonnées
       x = "âge") # idem, pour l'axe des abcisses.
# Histogramme des montants du dernier achat
g2 <- ggplot(data = dbm_sub,
             mapping = aes(x = x8)) +
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
  labs(y = "densité",
       x = "nombre d'achats différents depuis un an")

# Avec le paquet 'patchwork', on peut combiner les graphiques
(g1 + g2)/(g3 + g4) / (g5 + g6) + # + pour combiner par colonne, `/` pour séparer les lignes
  patchwork::plot_layout(guides = 'collect') & # combiner les légendes
  theme_classic() & # thème classique, sans grille ni fond gris
  theme(legend.position = "bottom") # déplacer la légende de droite vers le bas



## Sélectionner toutes les variables numériques
# et créer un tableau résumé pour chaque variable
dbm_sub_c <- dbm_sub |>
   # Sélection les variables
    dplyr::select(x2, x6:x10) |>
   # Fournir une liste avec noms et les fonctions à calculer
    dplyr::summarize_all(list(moy = mean, min = min, max = max))
# Pour imprimer dans un fichier Rmarkdown ou Quarto, on peut utiliser
# la fonction 'kable' du paquet 'knitr' pour formatter (arrondir, etc.)
dbm_sub_c |>
  knitr::kable(digits = 2, booktabs = TRUE, linesep = "")

#Créer la base de données d'entraînement
dbm_a <- dbm_sub |>
  dplyr::filter(!is.na(ymontant)) # personnes qui ont acheté


## Créer une formule avec tous les coefficients de régression possibles considérés
# (...)^2 crée toutes les interactions d'ordre deux
# I(x^2) permet de créer les termes quadratiques
formule <-
  formula(ymontant ~
          (x1 + x2 + x3 + x4 + x5 +
             x6 + x7 + x8 + x9 + x10)^2 +
            I(x2^2) + I(x6^2) + I(x7^2) +
            I(x8^2) + I(x9^2) + I(x10^2))
mod_complet <- lm(formule, data = dbm_a)
# Matrice du modèle avec toutes les variables obtenues par transformation (produit et termes quadratiques)
matmod <- model.matrix(mod_complet)


# Recherche exhaustive, ici avec uniquement les variables de base
rec_ex <- leaps::regsubsets(
  x = ymontant ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10,
  nvmax = 13L, # nombre maximum de termes à inclure 
  # avec les variables catégorielles, plusieurs coefficients
  method = "exhaustive", # choix de la méthode
  data = dbm_a) # nom de la base de données

# Imprimer un résumé de la sélection et des meilleurs modèles
resume_rec_ex <- summary(rec_ex,
                         matrix.logical = TRUE)
# La même chose, mais sous format graphique
# Variables incluses (oui/non) avec valeurs du BIC
plot(rec_ex)

# Trouver le modèle avec le plus petit BIC
min_BIC <- which.min(resume_rec_ex$bic)
# Nom des variables dans le modèle retenu
rec_ex$xnames[resume_rec_ex$which[min_BIC,]]




## Méthode de sélection séquentielle avec AIC
seq_AIC <- MASS::stepAIC(
  lm(ymontant ~ 1, data = dbm_a), # modèle de départ
  # le modèle initial n'inclut aucune variable explicative
    scope = formule, # modèle maximal possible
    direction = "both", #séquentielle
    trace = FALSE, # ne pas imprimer le suivi
    keep = function(mod, AIC, ...){
      # autres sorties des modèles à conserver
      list(bic = BIC(mod),
           coef = coef(mod))},
    k = 2) # pénalité pour critère d'information
# Remplacer k=2 par k = log(nrow(dbm_a)) pour BIC


## Sélection de variable et régression LASSO

# Créer une grille de pénalités
lambda_seq <- seq(from = 0.01, to = 2, by = 0.01)
# Ajuster le modèle pour toutes les valeurs de lambda_seq d'un coup
cv_output <-
   # Attention: la fonction `glmnet` prend une matrice de modèle 
   # et un vecteur de réponses
  glmnet::cv.glmnet(x = as.matrix(matmod),
            y = dbm_a$ymontant,
            alpha = 1, # garder cette valeur fixe à un pour le lasso
            lambda = lambda_seq)
#
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


# Pénalité qui minimise l'EQM de validation croisée
lambopt <- cv_output$lambda.min #ou cv_output$lambda.1se
## Une fois la pénalité choisie (lambopt), réajuster le modèle avec cette dernière
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
    test == 1, # Données de validation
    !is.na(ymontant)) # Uniquement le montant des personnes qui ont acheté
pred <- predict(lasso_best, # modèle duquel prédire
                s = lambopt, # valeur de la pénalité optimale
                newx = as.matrix( # matrice du modèle avec toutes les variables
                  model.matrix(formule, data = dbm_v)))
# Calcul de l'erreur quadratique moyenne à la mitaine
eqm_lasso <- mean((pred - dbm_v$ymontant)^2)

