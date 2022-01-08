exdir <- "../MATH60602/documents/exercices"
linkstring <- "https://nbviewer.jupyter.org/github/lbelzile/math60602/blob/master/documents/exercices/"
linkgithub <- "https://raw.githubusercontent.com/lbelzile/math60602/master/"

## exercise and solution files
fn <- FALSE


codesas <- list.files(path = exdir, pattern = "MATH60602_Exercice[[:digit:]].sas",
                      full.names = fn)

ex <- list.files(path = exdir, pattern = "MATH60602_Exercice[[:digit:]].pdf",
    full.names = fn)
so <- list.files(path = exdir, pattern = "MATH60602_Exercice[[:digit:]]_sol.pdf",
                      full.names = fn)

sas <- rep("", 7)
sas[as.integer(substr(codesas, start = 19, stop = 19))] <-
  paste0("[<span style='color: #4b5357;'><i class='fas fa-file-code fa-lg'></i></span>](", linkgithub, "documents/exercices/", codesas, ")")




## Numbers + Topics
# exid <- as.numeric(gsub("[^0-9.-]+", "", ex))
topics <-
  c("Analyse exploratoire",
    "Analyse factorielle",
    "Analyse de regroupements",
    "Sélection de modèles",
    "Régression logistique",
    "Analyse de survie",
    "Données manquantes")

exos <- c(paste0("[<span style='color: #4b5357;'><i class='fas fa-file-pdf fa-lg'></i></span>](", linkstring, ex, ")"),
  rep("", length.out = 7-length(ex)))
soln <- rep("", 7)
soln[as.integer(substr(so, start = 19, stop = 19))] <- c(paste0("[<span style='color: #bfc2c5;'><i class='far fa-file-pdf fa-lg'></i></span>](", linkstring, so, ")"))
exdat <- data.frame(Chapitre = topics,
                    Exercice = exos,
                    Solution = soln,
                    "SAS" = sas)

