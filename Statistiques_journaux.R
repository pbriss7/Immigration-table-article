################################
#### STATISTIQUES JOURNAUX #####
################################

# Données @Cision. Acquises à travers Eureka (McGill)
# Sources: La Presse, La Presse+, Le Devoir
# Période: 1992-2024
# Disponibilité variable des périodiques:
# Le Devoir: Archives start date:	1992-07-14
# La Presse: Archives start date:	1985-01-02 (mais seulement titres non exportables avant 1988)

# Critères avancés: 
# LEAD= (diversité* %10 Québec*)
# | LEAD= (immigr* %10 Québec*)
# | TIT_HEAD= (diversité* %10 Québec*)
# | TIT_HEAD= (immigr* %10 Québec*)
# | LEAD= (diversité* %10 Montréal*)
# | LEAD= (immigr* %10 Montréal*)
# | TIT_HEAD= (immigr* %10 Montréal*)


library(data.table)
library(ggplot2)

dt1 <- fread("donnees/Statistiques_journaux/ImmigrDivers19921995.csv")
dt2 <- fread("donnees/Statistiques_journaux/ImmigrDivers19961999.csv")
dt3 <- fread("donnees/Statistiques_journaux/ImmigrDivers20002004.csv")
dt4 <- fread("donnees/Statistiques_journaux/ImmigrDivers20052009.csv")
dt5 <- fread("donnees/Statistiques_journaux/ImmigrDivers20102014.csv")
dt6 <- fread("donnees/Statistiques_journaux/ImmigrDivers20152018.csv")
dt7 <- fread("donnees/Statistiques_journaux/ImmigrDivers20192022.csv")
dt8 <- fread("donnees/Statistiques_journaux/ImmigrDivers20232024.csv")

dt_all <- rbind(dt1, dt2, dt3, dt4, dt5, dt6, dt7, dt8)


table(dt_all$`Publication Title`)


dt_all[, .N, by = .(`Publication Year`)] |>
  ggplot(aes(x = `Publication Year`, y = N)) +
  geom_line() +
  geom_smooth() +
  labs(
    title = "Nombre d'articles publiés par année (1992-2024)",
    subtitle = "La Presse, La Presse+, Le Devoir",
    x = "Année de publication",
    y = "Nombre d'articles",
    caption = "Source : Cision"
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(size = 12),
    plot.subtitle = element_text(size = 10),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x  = element_text(size = 7),
    axis.text.y  = element_text(size = 7),
    plot.caption = element_text(size = 7)
  )+
  scale_x_continuous(
    breaks = seq(1992, max(dt_all$`Publication Year`, na.rm = TRUE), by = 5)
  )# +
  #  theme(
  #    axis.text.x = element_text(angle = 45, hjust = 1)
  #  )

ggsave("Graphiques_produits/Stats_journaux.png")



