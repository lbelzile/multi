## -----------------------------------------------------------------------------
library(ggplot2)
data(HMDA, package = "AER")
ggplot(data = HMDA[which(HMDA$pirat < 1 & HMDA$afam == "yes"),],
       aes(y = ifelse(deny == "yes", 1, 0), 
           x = pirat)) +
  geom_hline(yintercept = c(0,1), 
             alpha = 0.1, 
             linetype = 2) + 
  geom_point() +
  stat_smooth(method = "lm", 
              se = FALSE, 
              fullrange = TRUE,
              col = 4) +
  stat_smooth(method = "glm", 
              se = FALSE,
              fullrange = TRUE,
              method.args = list(family = "binomial"), col = 2) +
  labs(subtitle = "demande d'approbation pour crédit hypothécaire", 
       y = "",
       x = "ratio paiements sur revenus") +
  scale_y_continuous(breaks=c(0L,1L), 
                     limits = c(0,1)) + 
  scale_x_continuous(breaks = seq(0,1, by = 0.25), 
                     limits = c(0,1),
                     expand = c(0,0),
                     labels = c("0","0.25","0.5","0.75","1")) + 
  geom_text(data = tibble::tibble(
    x = c(1,0.05), 
    y = c(0.1,0.9), 
    label = c("refusée", "acceptée")), 
   aes(x = x, y = y, label = label),
   hjust = "inward") +
    theme_classic()


## -----------------------------------------------------------------------------
logit <- function(x){log(x/(1-x))}
expit <- function(x){1/(1+exp(-x))}
par(mar = c(4,4,1,0.1), bty = "l")
curve(expit, 
    ylim = c(0,1),
    yaxs="i",
    from = -3.5, 
    to = 3.5, 
    xlab = expression(eta), 
    ylab = expression(p))



## -----------------------------------------------------------------------------
## data(logit1, package = "hecmulti")
## # Ajustement du modèle avec toutes
## # les variables explicatives
## modele1 <- glm(formula = y ~ .,
##             family = binomial(link = "logit"),
##             data = logit1)


## -----------------------------------------------------------------------------
## summary(modele1)


## -----------------------------------------------------------------------------
par(mar = c(4,4,1,0.1), bty = "l")
curve(expit(-3.05 + 0.0749 * x), 
    ylim = c(0,1),
    yaxs = "i",
    from = 18, 
    to = 59, 
    xlab = "âge (en années)", 
    ylab = "p")



## -----------------------------------------------------------------------------
datf <- matrix("", nrow = 2, ncol = 10)
datf[1,] <- c("\\(p\\)", sprintf(seq(0.1,0.9, by = 0.1),fmt = "%.1f"))
datf[2,] <- c("cote", paste0("\\(",c("\\frac{1}{9}","\\frac{1}{4}","\\frac{3}{7}","\\frac{2}{3}","1","\\frac{3}{2}","\\frac{7}{3}","4","9"),"\\)"))
knitr::kable(datf[-1, , drop = FALSE], 
             col.names = datf[1, , drop = FALSE],
             row.names = FALSE,
             booktabs = TRUE,
             longtable = FALSE,
             align =  paste0(c("l",rep("c", 9)),collapse = ""),
             escape = FALSE,
             format = "latex") 


## -----------------------------------------------------------------------------
## modele2 <- glm(
##   formula = y ~ .,
##   data = logit1,
##   family = binomial)
## exp(coef(modele2))


## -----------------------------------------------------------------------------
modele2 <- glm(
  formula = y ~ .,
  data = hecmulti::logit1,
  family = binomial)
round(exp(coef(modele2)), 3)


## -----------------------------------------------------------------------------
## logit2 <- logit1 |>
##    mutate(x1 = relevel(x1, ref = 2))


## -----------------------------------------------------------------------------
knitr::include_graphics("figures/poupeesrusses.jpg")


## -----------------------------------------------------------------------------
modele2 <-  glm(y ~ x1 + x2 + x3 + x4 + x5 + x6,
                 data = hecmulti::logit1,
                 family = binomial(link = "logit"))
modele3 <-  glm(y ~ x1 + x2 + x3 + x4 + x5,
                 data = hecmulti::logit1,
                 family = binomial(link = "logit")) 


## -----------------------------------------------------------------------------
# modèle 2 (alternative), modèle 3 (nulle)
anova(modele3, modele2, test = "LR")
## Deviance = -2*log vraisemblance
rvrais <- modele3$deviance - modele2$deviance
pchisq(rvrais, df = 2, lower.tail = FALSE) # valeur-p


## -----------------------------------------------------------------------------
car::Anova(modele2, type = "3")


## -----------------------------------------------------------------------------
## confint(modele2)      # IC pour beta
## exp(confint(modele2)) # IC pour exp(beta)


## -----------------------------------------------------------------------------
data(logit1, package = "hecmulti")
modele2 <- glm(
  y ~ x1 + x2 + x3 + x4 + x5 + x6,
  data = logit1,
  family = binomial(link = "logit")
)
modelsummary::modelplot(modele2, 
                        exponentiate = TRUE,
       coef_omit = 'Interc') + 
  ggplot2::labs(x = paste0("exp(", expression(beta),")")) +
  ggplot2::geom_vline(xintercept = 1)




## -----------------------------------------------------------------------------
data(logit1, package = "hecmulti")
logit1 <- logit1 |> 
  dplyr::mutate(y = factor(y),
               x3 = factor(x3),
               x4 = factor(x4))
m1 <- glm(y ~ x1 + x2 + x3 + x4 + x5 + x6, 
          family = binomial(link = "logit"),
          data = logit1)
tbl <- m1 |>
  gtsummary::tbl_regression(
  exponentiate = TRUE,
  intercept = FALSE) |>
  gtsummary::add_global_p() |>
  gtsummary::bold_labels()
tbl$table_styling$header$label[c(12,20,26,27)] <- c("variables","cote", "IC 95%","valeur-p")
tbl$table_body <- tbl$table_body[1:15,]
# tbl$table_body$label[1] <- "cst"
tbl$table_styling$footnote_abbrev$footnote <- 
  c("cote = rapport de cote",
    "IC = intervalle de confiance",
    "ET = erreur-type")
tbl |> 
   gtsummary::as_gt()  |>
   gt::tab_options(table.width = gt::pct(100))


## -----------------------------------------------------------------------------
tbl <- m1 |>
  gtsummary::tbl_regression(
  exponentiate = TRUE,
  intercept = FALSE) |>
  gtsummary::add_global_p() |>
  gtsummary::bold_labels()
tbl$table_styling$header$label[c(12,20,26,27)] <- c("variables","cote", "IC 95%","valeur-p")
tbl$table_body <- tbl$table_body[-(1:15),]
# tbl$table_body$label[1] <- "cst"
tbl$table_styling$footnote_abbrev$footnote <- 
  c("cote = rapport de cote",
    "IC = intervalle de confiance",
    "ET = erreur-type")
tbl |> 
   gtsummary::as_gt()  |>
   gt::tab_options(table.width = gt::pct(100))


## -----------------------------------------------------------------------------
car::vif(modele2)

