
data(logit1, package = "hecmulti")
# Ajustement du modèle avec toutes
# les variables explicatives
modele1 <- glm(formula = y ~ .,
            family = binomial(link = "logit"),
            data = logit1)
# Coefficients, tests de Wald pour les coefficients
# avec mesures d'ajustement global
summary(modele1)

# Comme le modèle est multiplicatif
# le rapport de cote donne une
#  augmentation ou diminution de exp(beta)
exp(coef(modele2))


modele2 <-  glm(y ~ x1 + x2 + x3 + x4 + x5 + x6,
                 data = hecmulti::logit1,
                 family = binomial(link = "logit"))
modele3 <-  glm(y ~ x1 + x2 + x3 + x4 + x5,
                 data = hecmulti::logit1,
                 family = binomial(link = "logit"))


# modèle 2 (alternative), modèle 3 (nulle)
anova(modele3, modele2, test = "LR")
## Deviance = -2*log vraisemblance
rvrais <- modele3$deviance - modele2$deviance
pchisq(rvrais, df = 2, lower.tail = FALSE) # valeur-p

# Tests de rapport de vraisemblance
car::Anova(modele2, type = "3")

# Intervalles de confiance
# (vraisemblance profilée)
confint(modele2)      # IC pour beta
exp(confint(modele2)) # IC pour exp(beta)

# Facteurs d'inflation de la variance généralisés
car::vif(modele2)
