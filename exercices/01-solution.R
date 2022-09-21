## -------------------------------------------------------
# remotes::install_github("lbelzile/hecmulti")
data(aerien, package = "hecmulti")
# Consulter la fiche descriptive
# ? hecmulti::aerien


## -------------------------------------------------------
dplyr::glimpse(aerien)


## -------------------------------------------------------
library(dplyr)
# Remplacer valeurs manquantes (0 en NA)
aerien <- aerien |> 
  mutate_at(7:20, ~na_if(., 0))
  # Statistiques descriptives
summary(aerien)
  
# Voir décompte pour éléments du questionnaire
aerien |> 
  mutate_at(7:20, ~factor(., ordered = TRUE)) |>
  # Statistiques descriptives
  summary()

# Moyenne et écart-type par variable
# selon niveau de satisfaction
aerien |>
  group_by(satisfaction) |>
  summarize(across(where(is.numeric), 
            ~ mean(.x, na.rm = TRUE),
            .names = "moy_{.col}")) |> 
  # Pivoter tableau
  tidyr::pivot_longer(cols = -1,
                      names_to = "variable",
                      names_prefix = "moy_",
                      values_to = "moyenne") |>
  arrange(variable) |> # trier
  knitr::kable(digits = 2)


## -------------------------------------------------------
aerien |> 
  group_by(loyaute_consommateur, 
           classe) |> 
  summarise(cnt = n()) |>
  mutate(freq = formattable::percent(cnt / sum(cnt))) |>
  knitr::kable()


## -------------------------------------------------------
corrplot::corrplot(
  corr = cor(aerien[,7:20], 
             use = "pairwise.complete.obs"), 
  diag = FALSE,
  type = "upper", 
  tl.pos = "n") # pas de nom


## -------------------------------------------------------
library(ggplot2)
theme_set(theme_classic())
g1 <- ggplot(data = aerien, 
       aes(x = loyaute_consommateur,
           fill = satisfaction)) +
  geom_bar(position = "fill") +
  labs(y = "", 
       x = "loyauté du consommateur")

g2 <- aerien |>
  count(service_internet_en_vol, 
        satisfaction) |>
  group_by(satisfaction) |>
  mutate(pct = n / sum(n)) |>
  ggplot(aes(x = service_internet_en_vol,
             y = pct,
           fill = satisfaction)) +
  geom_bar(stat = "identity",
           position = "dodge2") +
  labs(y = "", 
       subtitle = "Pourcentage des réponses",
       x = "service internet en vol")

g3 <- aerien |>
  count(preenregistrement_en_ligne, 
        satisfaction) |>
  group_by(satisfaction) |>
  mutate(pct = n / sum(n)) |>
  ggplot(aes(x = preenregistrement_en_ligne,
             y = pct,
           fill = satisfaction)) +
  geom_bar(stat = "identity",
           position = "dodge2") +
  labs(y = "", 
       subtitle = "Pourcentage des réponses",
       x = "préenregistrement en ligne")

g4 <- ggplot(data = aerien, 
       aes(x = delai_arrivee_min,
           fill = satisfaction)) +
  geom_histogram(boundary = 0,
                 binwidth = 2,
                 position = "dodge2") +
  coord_trans(x = 'log1p',
              y = 'log1p') +
  labs(y = "", 
       x = "délai à l'arrivée (en minutes)")

g5 <- ggplot(data = aerien,
             aes(x = delai_depart_min,
                 y = delai_arrivee_min),
             alpha = 0.5) +
  geom_point() +
  labs(x = "délais au départ (en minutes)",
       y = "délais à l'arrivée (en minutes)") +
  coord_trans(x = "log1p",
              y = "log1p")

library(patchwork)
# Imprimer plusieurs graphiques
(g1 + g2) / (g3 + g4) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

g5

