marathon <- read.csv(
  "/home/lbelzile/Documents/Website/multi/files/data/MarathonBerlin2022.csv"
)

# Manipulation de base de données

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(countrycode)
# Transformez les types de variables (temps, facteurs, entiers)
# Obtenez les noms associés aux codes des pays (codes du CIO)
# Transformez en valeurs manquantes
#  - les valeurs de sexe non déclarées
#  - les classements des personnes qui n'ont pas complété la course, ou qui ont été disqualifiées
# Retirez les variables `starttime`, `grosstime` et `division`
# Créez un facteur `vague` représentant le temps de la vague de départ (`starttime`)
head(sort(table(marathon$position_gender), decreasing = TRUE))
head(sort(table(marathon$position), decreasing = TRUE))


berlin <- marathon |>
  mutate(
    position = as.integer(
      na_if(position, "DNF")
    ),
    position_gender = as.integer(
      replace_values(position_gender, c("–", "DSQ") ~ NA)
    ),
    countrycode = case_when(nchar(country) == 3L ~ country, .default = NA),
    country = recode_values(
      countrycode,
      from = countrycode::codelist$ioc,
      to = countrycode::codelist$country.name.fr,
      default = NA
    ),
    sex = factor(na_if(sex, "–")),
    bib = factor(bib),
    category = factor(category),
    vague = factor(substr(start_raw_time, 1, 5)),
    half = na_if(half, "")
  ) |>
  filter(!is.na(position_gender)) |>
  mutate(
    start_raw_time = hms(na_if(start_raw_time, "–"), quiet = TRUE),
    across(half:nettime, ~ as.duration(hms(na_if(.x, "–"), quiet = TRUE))),
  ) |>
  select(
    !c(
      division, # pas utilisée
      grosstime, # information déjà avec nettime
      starttime # transformé en facteur (vague)
    )
  )
berlin <- berlin |>
  mutate(
    V5 = 5 / as.numeric(X5k) * 3600,
    V10 = 5 / as.numeric(X10k - X5k) * 3600,
    V15 = 5 / as.numeric(X15k - X10k) * 3600,
    V20 = 5 / as.numeric(X20k - X15k) * 3600,
    V25 = 5 / as.numeric(X25k - X20k) * 3600,
    V30 = 5 / as.numeric(X30k - X25k) * 3600,
    V35 = 5 / as.numeric(X35k - X30k) * 3600,
    V40 = 5 / as.numeric(X40k - X35k) * 3600,
    Vmoy = 42.195 / as.numeric(nettime) * 3600,
  ) |>
  mutate(across(starts_with("V", ignore.case = FALSE), round, digits = 2))

1. Créez un diagramme à bande du nombre de participants par pays (10 premiers)
2. Tracez un histogramme des temps de complétion du marathon en fonction du sexe de l'athlète
3. Créez un diagramme spaghetti (ligne brisée) de la vitesse des 20 coureurs les plus rapides en fonction de la distance par intervalle de 5km (*difficile*).

top25 <- berlin |>
  select(position, starts_with("V", ignore.case = FALSE)) |>
  select(!Vmoy) |>
  filter(position <= 25) |>
  tidyr::pivot_longer(
    cols = tidyr::starts_with("V"),
    names_to = "km",
    names_prefix = "V",
    values_to = "speed"
  ) |>
  mutate(km = as.numeric(km))

library(tinyplot)
tinytheme("clean2")
plt(
  x = speed ~ km | position,
  type = "b",
  xlab = "km",
  ylab = "",
  main = "Performance des 25 meilleurs coureurs",
  sub = "Vitesse (km/h) par segment de 5km",
  cap = "Source: Marathon de Berlin, 2022",
  data = top25,
  legend = NULL
)

theme_set(theme_classic())
ggplot(
  data = top25,
  mapping = aes(
    x = km,
    group = factor(position),
    col = factor(position),
    y = speed
  )
) +
  geom_line(show.legend = FALSE) +
  labs(
    x = "km",
    y = "",
    #     col = "rang",
    subtitle = "Vitesse (km/h) par segment de 5km",
    title = "Performance des 25 meilleurs coureurs",
    caption = "Source: Marathon de Berlin, 2022",
  )


# Facteur, avec ordre par nombre
pays10 <- as.data.frame(
  sort(with(berlin, table(country)), decreasing = TRUE)[10:1]
) |>
  mutate(country = factor(country, levels = country, labels = country))

ggplot(
  data = pays10,
  mapping = aes(
    y = country,
    x = Freq
  )
) +
  geom_col() +
  scale_x_continuous(limits = c(0, NA), expand = expansion()) +
  labs(
    y = "",
    x = "nombre de participants par pays",
    caption = "Marathon de Berlin (2022)"
  )

plt(
  Freq ~ country,
  data = pays10,
  type = "barplot",
  main = "Nombre de participants par pays",
  xlab = "",
  ylab = "",
  flip = TRUE,
  cap = "Source: Marathon de Berlin, 2022"
)
