
linkgithub <- "https://raw.githubusercontent.com/lbelzile/math60602/master/documents/codeSAS"

## exercise and solution files
fn <- FALSE
chap <- c(1,2,3,4,4,5,5,5,5,6,7,7)
codeSAS <- paste0(c("explo1", "factor", "cluster", "selection1_intro","selection2_methodes","logit.sas","logit3_lift_chart", "logit6_macro_all_subset", "logit4_macro_gain_avec_class", "survie.sas","manquantes1","manquantes2_prevision"),".sas")
# codeSAS <- list.files(path = codedir, pattern = ".sas",
#                     full.names = fn)



topics <-
  c("Analyse exploratoire",
    "Analyse factorielle",
    "Analyse de regroupements",
    "Sélection de modèles (introduction)",
    "Sélection de modèles (méthodes)",
    "Régression logistique",
    "macro (courbe lift)",
    "macro (recherche exhaustive)",
    "macro (calcul du gain)",
    "Analyse de survie",
    "Données manquantes (introduction)",
    "Données manquantes (prédiction)")
rc <- paste0("[",topics,"](", linkgithub, "/", codeSAS, ")")
SAScode <- data.frame(Chapitre = chap,
                    Description = rc)
