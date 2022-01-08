exdir <- "../MATH60602/documents/codeR/"
linkgithub <- "https://raw.githubusercontent.com/lbelzile/math60602/master/documents/codeR"

## exercise and solution files
fn <- FALSE

coder <- list.files(path = exdir, pattern = "MATH60602-0",
                      full.names = fn)



topics <-
  c("Analyse exploratoire",
    "Analyse factorielle",
    "Analyse de regroupements",
    "Sélection de modèles",
    "Régression logistique",
    "Analyse de survie",
    "Données manquantes")
rc <- paste0("[",topics, "](", linkgithub, "/", coder, ")")

Rcode <- data.frame(Chapitre = 1:7,
                    Description = rc)

