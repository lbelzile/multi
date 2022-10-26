## ---- fig.width = 7, fig.height=5, out.width = '75%', fig.align = "center"----
set.seed(60602)
n <- 100L
x <- runif(n = n)
y <- 2 + x*5 + rnorm(n, sd = 0.5)
library(ggplot2)
ggplot(data.frame(x = x, y = y),
       aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", 
              se = FALSE, 
              col ="black",
              formula = y ~ x,
              fullrange = TRUE) +
  theme_classic()



## ----distancevert, eval = TRUE, echo = FALSE, fig.width=7, fig.height=5, out.width = '80%', fig.align = 'center'----
ols <- lm(y ~ x)
vlines <- data.frame(x = x, y = y, y2 = fitted(ols))
ggg <- ggplot2::ggplot(data = vlines,
                       aes(x = x, y = y)) +
   geom_point() +
   geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      col = "black",
      fullrange = TRUE
   ) +
   labs(x = "variable explicative",
        y = "réponse") +
   geom_segment(mapping = aes(
      x = x, 
      y = y2,
      xend = x,
      yend = y), 
   arrow = arrow(length = unit(0.2,"cm")),
   show.legend = FALSE) +
   theme_classic()
print(ggg)


## ---- echo = TRUE, eval = FALSE-----------------------------------------------
## modlin <- lm(mpg ~ hp + wt,
##              data = mtcars)
## summary(modlin)


## ---- eval = TRUE, echo = FALSE-----------------------------------------------
modlin <- lm(mpg ~ hp + wt, 
             data = mtcars)
summary(modlin)


## ---- eval = FALSE, echo = TRUE-----------------------------------------------
## data(polynome, package = "hecmulti")
## k <- 4L
## lm(y ~ poly(x, degree = k),
##    data = polynome)


## ---- fig.height=5, fig.width=7, out.width='70%', fig.align = "center"--------
data(polynome, package = "hecmulti")
library(ggplot2)
ggplot(data = polynome, 
       mapping = aes(x=x, y=y)) +
 geom_point() +
 theme_classic()


## -----------------------------------------------------------------------------
data(polynome, package = "hecmulti")
x0 <- seq(from = -6, to = 6, length.out = 1001L)

mod <- lm(y ~ poly(x, degree = 10), data = polynome)
ypred10 <- predict(mod, newdata = data.frame(x = x0))

mod <- lm(y ~ poly(x, degree = 4), data = polynome)
ypred4 <- predict(mod, newdata = data.frame(x = x0))

mod <- lm(y ~ poly(x, degree = 1), data = polynome)
ypred1 <- predict(mod, newdata = data.frame(x = x0))
library(ggplot2)
ggplot() +
   geom_point(data = polynome,
              aes(x = x, y = y)) +
   geom_line(data = data.frame(x0, ypred1),
              aes(x = x0, y = ypred1), color = 2) +
   geom_line(data = data.frame(x0, ypred4),
              aes(x = x0, y = ypred4), color = 3) +
   geom_line(data = data.frame(x0, ypred10),
              aes(x = x0, y = ypred10), color = 4) + 
   theme_classic() +
   scale_y_continuous(limits = c(-400,400))


## ---- eval = FALSE, echo = TRUE-----------------------------------------------
## data(polynome, package = "hecmulti")
## eqm <- vector(length = 10L, mode = "numeric")
## for(K in seq_len(10)){
##  mod <- lm(y ~ poly(x, degree = K), data = polynome)
##  eqm[K] <- mean(resid(mod)^2)
## }


## ---- fig.width = 5, fig.height = 4, out.width = '70%', cache = TRUE, fig.align = 'center'----

file <- "https://lbelzile.bitbucket.io/MATH60602/selection1_test.sas7bdat"
test <- haven::read_sas(data_file = file)
train <- polynome

EQM <- matrix(0, nrow = 10, ncol = 7)
for(i in seq_len(10)){
  meanmod <- as.formula(paste0("y~", paste0("I(x^",1 :i,")", collapse= "+")))
  mod <-  lm(meanmod, data = train)
  # Calculer l'erreur moyenne dans les deux échantillons
  EQM[i,1] <- mean(resid(mod)^2) #apprentissage
  EQM[i,2] <-  mean((test$y - predict(mod, newdata = test))^2)
}
EQMdat <- data.frame(
  ordre = rep(1:10, length.out = 20),
           EQM = c(EQM[,1:2]),
           echantillon =
    factor(rep(c("apprentissage","théorique"), each = 10)))
ggplot(data = EQMdat, aes(x=ordre, y=EQM, color=echantillon)) +
  geom_line() +
  geom_point(aes(shape=echantillon, color=echantillon)) +
  labs(x = "ordre du polynôme",
       subtitle = "erreur quadratique moyenne",
       y = "",
       color = "",
       shape = "") +
  theme(legend.position="bottom",
        legend.title=element_blank()) +
  scale_color_manual(values = MetBrewer::met.brewer("Hiroshige",
                                           n =  2)) +
  scale_x_continuous(breaks = 0:10,
                     labels = as.character(0:10)) +
  theme_classic() + 
  theme(legend.position = c(0.8, 0.8))


## ----overfitting, echo = FALSE, out.width = ".7\\linewidth", fig.align ='center'----
hecblue <- rgb(red = 0, green = 60, blue = 113, max = 255)
heccyan <- rgb(red = 0, green = 159, blue = 223, max = 255)
make_poly_data = function(sample_size = 11) {
  x = seq(0, 10)
  y = 3 + x + 4 * x ^ 2 + rnorm(n = sample_size, mean = 0, sd = 20)
  data.frame(x, y)
}
set.seed(1234)
poly_data = make_poly_data()
fit_quad = lm(y ~ poly(x, degree = 2), data = poly_data)
fit_big  = lm(y ~ poly(x, degree = 10), data = poly_data)

plot(y ~ x, data = poly_data, ylim = c(-100, 400), cex = 2, pch = 20)
xplot = seq(0, 10, by = 0.1)
lines(xplot, predict(fit_quad, newdata = data.frame(x = xplot)),
      col = hecblue, lwd = 2, lty = 1)
lines(xplot, predict(fit_big, newdata = data.frame(x = xplot)),
      col = heccyan, lwd = 2, lty = 2)


## -----------------------------------------------------------------------------
lmkfold <- function(formula, data, k, ...){
  accu <- 0
  k <- as.integer(k)
  n <- nrow(data)
  gp <- sample.int(n, n, replace = FALSE)
  folds <- split(gp, cut(seq_along(gp), k, labels = FALSE))
  for(i in 1 :k){
   g <- as.integer(unlist(folds[i]))
   fitlm <- lm(formula, data[-g,])
   accu <- accu + sum((data[g, all.vars(formula)[1]] -predict(fitlm, newdata=data[g,]))^2)
  }
return(accu/n)
}

EQM <- matrix(0, nrow = 10, ncol = 7)
EQMcv <- matrix(0, nrow = 10, ncol = 100)
suppressPackageStartupMessages(library(caret))
library(ggplot2)
for(i in 1 :10){
  set.seed(i*1000)
  # Créer le modèle avec une chaîne de caractère pour le polynôme
  meanmod <- as.formula(paste0("y~", paste0("I(x^",1 :i,")", collapse= "+")))
  mod <-  lm(meanmod, data = train)
  # Calculer l'erreur moyenne dans les deux échantillons
  EQM[i,1 :2] <- c(mean(resid(mod)^2), #apprentissage
               mean((test$y - predict(mod, newdata = test))^2)) #échantillon test
  EQM[i,3] <- summary(mod)$r.squared
  EQM[i,4] <- summary(mod)$adj.r.squared
  EQM[i,5] <- AIC(mod)
  EQM[i,6] <- BIC(mod)
# validation croisée avec 10 groupes
  EQMcv[i,] <-  replicate(n = 100L,
                    train(form = meanmod, data = train, method = "lm",
                trControl = trainControl(method = "cv",
                                         number = 10))$results$RMSE^2)
  # EQM[i,7] <- lmkfold(formula = meanmod, data = train, k = 10)
}
  EQM[,7] <- rowMeans(EQMcv)


EQMdat <- data.frame(ordre = rep(1 :10, length.out = 20),
           EQM = c(EQM[,1 :2]),
           echantillon = factor(c(rep("apprentissage",10), rep("théorique", 10)))
)


## -----------------------------------------------------------------------------
EQM_sub <- EQM[,c(1,2,5,6)]
colnames(EQM_sub) <- c("echantillon",
                       "validation", 
                       "AIC",
                       "BIC")
EQM_graph <- data.frame(ordre = 1:10, EQM_sub[,3:4]) |>
  tidyr::pivot_longer(cols = -1,
                      names_to = "methode",
                      values_to = "eqm") # |>
  # dplyr::mutate(methode = forcats::lvls_reorder(
  #   factor(methode), idx = c(3L,4L,1L,2L)))
ggplot(data = EQM_graph, 
       aes(x = ordre, 
           y = eqm, 
           color = methode)) +
  geom_line() +
  geom_point(aes(shape=methode, color=methode)) +
  labs(x = "ordre du polynôme",
       y = "",
       subtitle = "") +
  scale_color_manual(values=MetBrewer::met.brewer("Hiroshige", 2)) +
  scale_x_continuous(breaks = 0:10,
                     labels = as.character(0:10)) +
  theme_classic() +
  theme(legend.position="bottom",
        legend.title=element_blank())


## -----------------------------------------------------------------------------
col <- rep(1, 25)
col[(0:4)*5+1:5] <- 2
df <- data.frame(
  x = rep(1:5, 5),
  y = rep(1:5, each = 5),
  identification = factor(col, labels = c("apprentissage","validation"))
)
ggplot(df, aes(x, y)) +
  geom_tile(aes(fill = identification), colour = "white", lwd = 2) +
  ylab("étape") +
  xlab("division") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  scale_color_manual(values=MetBrewer::met.brewer("Hiroshige", 2)) +
  scale_fill_manual(values=MetBrewer::met.brewer("Hiroshige", 2)) +
  scale_y_continuous(
    breaks = 1:5,
    minor_breaks = NULL,
    expand = c(0,0),
    labels = 5:1)



## ---- echo = TRUE, eval = FALSE-----------------------------------------------
## cv_caret <-
##   caret::train(form = formula(y ~ poly(x, degree = 3)),
##              data = polynome,
##              method = "lm",
##              trControl = caret::trainControl(
##                method = "cv",
##                number = 10)) #nb plis
## reqm_cv <- cv_caret$results$RMSE # racine EQM
## reqm_sd_cv <- cv_caret$results$RMSESD


## -----------------------------------------------------------------------------
EQMdat <- data.frame(ordre = rep(1:10, length.out = 20),
           EQM = c(EQM[,2], rowMeans(EQMcv)),
           echantillon = factor(c(rep("théorique",10), rep("validation croisée", 10)))
)
ggplot(data = EQMdat, aes(x=ordre, y=EQM)) +
  geom_boxplot(data = data.frame(EQM = c(t(EQMcv)),
                                 ordre = rep(1:10, each=100)),
               aes(group=ordre),
               show.legend = FALSE,
               outlier.shape=NA) +
  #geom_line(aes(color=color=echantillon)) +
  geom_point(aes(shape=echantillon, color=echantillon)) +
  labs( x = "ordre du polynôme",
        y = "",
     subtitle = "") +
  theme_classic() + 
  theme(legend.position="bottom",
        legend.title=element_blank()) +
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  scale_x_continuous(breaks = 0:10,
                     labels = as.character(0:10))


