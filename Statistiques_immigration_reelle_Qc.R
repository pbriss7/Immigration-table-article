####################################
#### STATISTIQUES IMMIGRATION #####
####################################

library(data.table)
library(ggplot2)
library(scales)
library(gt)
library(kableExtra)
library(tidyr)

df <- fread("donnees/Statistiques_officielles/Solde_migratoire.csv")
setDT(df)

df[, Solde_migratoire_total:=NULL]
df <- df |>
  mutate(
    Annee = as.integer(Annee),
    Nombre = as.integer(Immigrants),
    Solde_migratoire = as.integer(Solde_migratoire_international)
  ) |>
  mutate(
    period_5y = paste0(floor(Annee / 5) * 5, "-", floor(Annee / 5) * 5 + 4)
  )

df[, Solde_migratoire_international:=NULL]
df[, Immigrants:=NULL]

# Agrégation sur 5 ans
df_agg <- df |>
  group_by(period_5y) |>
  summarise(
    immigrants = sum(Nombre),
    solde = sum(Solde_migratoire),
    .groups = "drop"
  )

# Transformation en format long
df_long <- df_agg |>
  pivot_longer(
    cols = c(immigrants, solde),
    names_to = "type",
    values_to = "valeur"
  )

# Graphique avec légende
ggplot(df_long, aes(x = period_5y, y = valeur, group = type)) +
  geom_col(
    data = df_long |> filter(type == "immigrants"),
    aes(fill = "Immigrants"),
    width = 0.6
  ) +
  geom_line(
    data = df_long |> filter(type == "solde"),
    aes(color = "Solde migratoire"),
    size = 1.2
  ) +
  geom_point(
    data = df_long |> filter(type == "solde"),
    aes(color = "Solde migratoire"),
    size = 2
  ) +
  scale_y_continuous(
    labels = label_comma(big.mark = " ", decimal.mark = ",")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Immigrants" = "#1f77b4")
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Solde migratoire" = "#ff7f0e")
  ) +
  labs(
    title = "Immigration internationale par période de 5 ans",
    x = NULL,
    y = "Nombre de personnes",
    caption = "Source : Statistique Canada\n\n"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=1))

ggsave(
  filename = "Graphiques_produits/immigrationQc.png",  # ou .pdf, .svg, etc.
  width = 10, height = 6, units = "in",       # taille du graphique
  dpi = 400                                   # résolution (utile pour les images)
)



########################################################
#### Distribution population immigrante régions Qc ----
########################################################

regions <- fread("donnees/Statistiques_officielles/portraits-regionaux-2012-2021-presence-2023.csv")
regions[, n_personnes_presentes_2012_2021:=str_remove_all(n_personnes_presentes_2012_2021, "\\s")]

regions[str_detect(n_personnes_presentes_2012_2021, "^[a-zA-Z]+")]
regions[n_personnes_presentes_2012_2021 == "x", n_personnes_presentes_2012_2021:=0]
regions[, n_personnes_presentes_2012_2021:=as.numeric(n_personnes_presentes_2012_2021)]

dt_regroupee <- regions[, .(Total = sum(as.integer(n_personnes_presentes_2012_2021))), by = Region][order(-Total)]

dt_regroupee[dt_regroupee$Region == "Montréal"]$Total + sum(dt_regroupee[2:15]$Total)
sum(dt_regroupee$Total[1:4]) /  sum(dt_regroupee$Total[1:15])

repartition_immigration_quebec <- dt_regroupee |> 
  kbl(booktabs = T,
      caption = "Répartition des personnes immigrantes au Québec 2012-2021") |> 
  kable_styling(latex_options = c("striped", "scale_up")) |>
  column_spec(2, color = "white", 
              background = spec_color(-dt_regroupee$Total, end = 0.7)) # |> 
  #add_footnote("Source: Ministère de l'Immigration, de la Francisation et de l'Intégration du Québec / Données Québec",
  #             notation = "symbol")

# install.packages("webshot2")
library(webshot2)
save_kable(repartition_immigration_quebec, "Graphiques_produits/immigration_repartition_quebec.png")



