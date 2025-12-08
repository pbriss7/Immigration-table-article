################################################
#### CRÉATION DES STATISTIQUES ROMANS@LIRE #####
################################################


library(data.table)
library(dplyr)
library(ggplot2)
library(gt)
library(stringr)
library(tidyr)


ralQc <- readRDS("donnees_traitees/ralQc.RDS")
ralQcMtlSeul <- readRDS("donnees_traitees/ralQcMtlSeul.RDS")
ralQcNonMtl <- readRDS("donnees_traitees/ralQcNonMtl.RDS")
ralQcMtl_Mixte <- readRDS("donnees_traitees/ralQcMtl.RDS")


#### Distribution chrono du roman québécois ----
ralQc[Pays_cat == "Québec" & Annee_publication >=1980 & Annee_publication <=2024, .N, by = .(Annee_publication)] |>
  ggplot(aes(x = Annee_publication, y = N)) +
  geom_bar(stat = "identity") +
  geom_smooth() +
  labs(
    title = "Romans à lire: nombre d'ouvrages québécois indexés",
    subtitle = "1980-2024",
    x = "Année de publication",
    y = "Nombre de titres",
    caption = "Source : Romans à lire, BAnQ"
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(size = 10),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x  = element_text(size = 7, hjust = 1),
    axis.text.y  = element_text(size = 7),
    plot.caption = element_text(size = 7)
    )+
  scale_x_continuous(
    breaks = seq(1980, max(ralQc$Annee_publication, na.rm = TRUE), by = 5)
  )

ggsave("Graphiques_produits/Distrib_chrono_Qc_19802024.png", dpi = 300)



# #### Distribution chronologique des romans selon le lieu du récit ----
# 
# ##### Roman montréalais (seul) ----
# 
# # Distribution annuelle des romans dont le seul lieu est Montréal
# distrib_annuelle_montreal <- ralQcMtlSeul[between(Annee_publication, 1925, 2024), .N, by = .(Annee_publication)][order(-Annee_publication)]
# 
# 
# # Distribution annuelle des romans québécois
# ralQc <- readRDS("donnees_traitees/ralQc.RDS")
# 
# distrib_annuelle_totale <- ralQc[between(Annee_publication, 1925, 2024), .N, by = .(Annee_publication)][order(-Annee_publication)]
# 
# 
# # Calculer la valeur relative en pourcentage
# distrib_annuelle_montreal <- merge(distrib_annuelle_montreal, distrib_annuelle_totale, by = "Annee_publication")
# distrib_annuelle_montreal <- rename(distrib_annuelle_montreal, N_montreal = N.x, N_totale = N.y)
# distrib_annuelle_montreal$Valeur_relative <- (distrib_annuelle_montreal$N_montreal / distrib_annuelle_montreal$N_totale) * 100
# 
# ggplot(distrib_annuelle_montreal[between(Annee_publication, 1925, 2024)], aes(x = Annee_publication, y = Valeur_relative, fill = Valeur_relative)) +
#   geom_col(width = 1, color = "blue", show.legend = FALSE, alpha=0.2) +  # Utiliser geom_col pour les barres
#   geom_smooth()+
#   labs(title = "Distribution chronologique relative des romans montréalais par rapport à l'ensemble",
#        x = "Année",
#        y = "Pourcentage",
#        caption = "Source: BAnQ, Romans@lire, 2024") +
#   theme_minimal() +
#   scale_y_continuous(labels = scales::percent_format(scale = 1))+
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(plot.caption = element_text(hjust = 0, size = 8, face = "italic"))
# 


#### Romans thématiques (alimentation, diversité, immigration) ----

regex_lieux <- "(?i)(Magasins?|Commerces?|Services?|Comptoirs?)\\b.{0,30}\\b(d[' ])?(alimentation?|rapide)|Restaura|Restos?\\b|\\bBars?\\b|Casse[- ]cro[ûu]tes?|Cantines?|Cafétérias?|Cafés?|Boulangeries?|Épiceries?|Supermarchés?|Dépanneurs?|Cabanes? (à|a) sucre|Salons? de thé|Tavernes?|Brasseries?|Marchés? (Jean-Talon|Atwater|Maisonneuve)"

regex_sujets <- "Gastronomies?|Nourritures?|Restaurations?"

regex_personnages <- "Serveurs?|Serveuses?|Barmaid|Barmans?|Cuisini[èe]re?s?|Sommeli[èe]rs?|Restaurateurs?"

regex_alimentation <- paste(regex_lieux, regex_sujets, regex_personnages, sep = "|")

pattern_alimentation <- regex(regex_alimentation, ignore_case = TRUE)

rm(regex_lieux)
rm(regex_sujets)
rm(regex_personnages)
rm(regex_alimentation)

# Filtrage des titres avec la regex
Alimentation_Montreal_seul <- ralQcMtlSeul[
  str_detect(Vedettes, pattern_alimentation) %in% TRUE | 
    str_detect(Categorie_personnage, pattern_alimentation) %in% TRUE | 
    str_detect(Lieu_geographique, pattern_alimentation) %in% TRUE,
  .(Id, Titre, Vedettes, Categorie_personnage, Lieu_geographique)
]

# table avec Montréal + autre chose, mais sans Montréal strict
Alimentation_Montreal_nonExclusif <- ralQcMtl_Mixte[
  str_detect(Vedettes, pattern_alimentation) %in% TRUE | 
    str_detect(Categorie_personnage, pattern_alimentation) %in% TRUE | 
    str_detect(Lieu_geographique, pattern_alimentation) %in% TRUE,
  .(Id, Titre, Vedettes, Categorie_personnage, Lieu_geographique)
]


# Table des Régions, i.e. au Québec, mais sans Montréal comme lieu
Alimentation_regions <- ralQcNonMtl[
  str_detect(Vedettes, pattern_alimentation) %in% TRUE | 
    str_detect(Categorie_personnage, pattern_alimentation) %in% TRUE | 
    str_detect(Lieu_geographique, pattern_alimentation) %in% TRUE,
  .(Id, Titre, Vedettes, Categorie_personnage, Lieu_geographique)
]


### Analyse des descripteurs du roman montréalais de la table ----
source("Fonctions.R")
personnage_table_mtl_dt <- freq_descripteurs(Alimentation_Montreal_seul, 
                             cols = c("Categorie_personnage"),
                             combine = TRUE)

vedettes_table_mtl_dt <- freq_descripteurs(Alimentation_Montreal_seul, 
                                                  cols = c("Vedettes"),
                                                  combine = TRUE)

sujets_table_mtl_dt <- vedettes_table_mtl_dt[!descripteur %in% personnage_table_mtl_dt$descripteur]

# Tables pour Word

gt(data = personnage_table_mtl_dt[1:12]) |> 
   tab_header(
     title = "Roman montréalais de la table (56)",
     subtitle = "Principaux types de personnages"
   ) |> 
  cols_label(descripteur = md("**Descripteur**"),
             "freq" = md("**Fréquence**")
  ) |> 
  tab_source_note(source_note = "Source: Romans à lire, BAnQ")

gt(data = sujets_table_mtl_dt[1:13]) |>
  tab_header(
    title = "Roman montréalais de la table (56)",
    subtitle = "Principaux sujets"
  ) |> 
  cols_label(descripteur = md("**Descripteur**"),
             "freq" = md("**Fréquence**")
             ) |> 
  tab_source_note(source_note = "Source: Romans à lire, BAnQ")


### Analyse des descripteurs du roman des régions de la table ----
personnage_table_regions_dt <- freq_descripteurs(Alimentation_regions, 
                                   cols = c("Categorie_personnage"),
                                   combine = TRUE)

vedettes_table_regions_dt <- freq_descripteurs(Alimentation_regions, 
                                 cols = c("Vedettes"),
                                 combine = TRUE)

sujets_table_regions_dt <- vedettes_table_regions_dt[!descripteur %in% personnage_table_regions_dt$descripteur]


# Tables pour Word
gt(data = personnage_table_regions_dt[1:12]) |> 
  tab_header(
    title = "Roman régional de la table (63)",
    subtitle = "Principaux types de personnages"
  ) |> 
  cols_label(descripteur = md("**Descripteur**"),
             "freq" = md("**Fréquence**")
  ) |> 
  tab_source_note(source_note = "Source: Romans à lire, BAnQ")

gt(data = sujets_table_regions_dt[1:12]) |>
  tab_header(
    title = "Roman régional de la table (63)",
    subtitle = "Principaux sujets"
  ) |> 
  cols_label(descripteur = md("**Descripteur**"),
             "freq" = md("**Fréquence**")
  ) |> 
  tab_source_note(source_note = "Source: Romans à lire, BAnQ")



#### Romans de la diversité/immigration ----
## Regex -------------------------------------------------


regex_immigration_diversite <- stringr::regex(
  "migrant|exilé|réfugié|demandeur.{1,6}d'asile|sans.papier
  |\\bmigrant|néo.québécois
  |(québécois|canadien|groupe|communauté|famille|femme|homme|artiste|travailleur|enfant|musulman).{1,10}(d'origine|racisé|ethnique|africain|asiatique|européen|américain|latino)
  |afghan|africain|algérien|Allemands?|Américains?|Antillais|Arabes?|Argentins?|Arméniens?
  |Asiatiques?|Australiens?|Autrichiens?|Belges?|Boliviens?|Brésiliens?|Cambodgiens?|Camerounais
  |Chiliens?|Chinois|Colombiens?|Congolais|Coréens?|Créoles?|Cubains?
  |Égyptiens?|Espagnols?|Éthiopiens?|Français
  |Grecs?|Guadeloupéens?|Guatémaltèques?|Haïtiens?|Indiens?|Irlandais
  |Islandais?|Israéliens?|Italiens?|Jamaïcains?|Japonais
  |Libanais?|Maghrébins?|Marocains?|Mauriciens?|Mexicains?
  |Péruviens?|Polonais|Portugais
  |Roumains?|Russes?|Salvadoriens?|Scandinaves?|Sénégalais|Siciliens?|Sierra-Léonais
  |Slaves?|Soviétiques?|Suisses?|Syriens?
  |Tchécoslovaques?|Tchèques?|Togolais|Tunisiens?|Turcs?
  |Ukrainiens?|Vietnamiens?|Yéménites?|Yougoslaves?
  ",
  comments    = TRUE,
  ignore_case = TRUE
)


## 2. Filtrage ------------------------------------------------------

Immigration_Montreal_seul <- ralQcMtlSeul[
  str_detect(Vedettes, regex_immigration_diversite) %in% TRUE | 
    str_detect(Categorie_personnage, regex_immigration_diversite) %in% TRUE | 
    str_detect(Lieu_geographique, regex_immigration_diversite) %in% TRUE,
  .(Id, Titre, Vedettes, Categorie_personnage, Lieu_geographique)
]


# Filtrage des titres avec la regex
Immigration_regions <- ralQcNonMtl[
  str_detect(Vedettes, regex_immigration_diversite) %in% TRUE | 
    str_detect(Categorie_personnage, regex_immigration_diversite) %in% TRUE | 
    str_detect(Lieu_geographique, regex_immigration_diversite) %in% TRUE,
  .(Id, Titre, Vedettes, Categorie_personnage, Lieu_geographique)
]


Immigration_Montreal_mixte <- ralQcMtl_Mixte[
  str_detect(Vedettes, regex_immigration_diversite) %in% TRUE | 
    str_detect(Categorie_personnage, regex_immigration_diversite) %in% TRUE | 
    str_detect(Lieu_geographique, regex_immigration_diversite) %in% TRUE,
  .(Id, Titre, Vedettes, Categorie_personnage, Lieu_geographique)
]


## 3. Table des descripteurs ------------------------------------------------------
source("Fonctions.R")

# Montréal seul
personnage_Immigr_Mtl_dt <- freq_descripteurs(Immigration_Montreal_seul, 
                                   cols = c("Categorie_personnage"),
                                   combine = TRUE)

vedettes_Immigr_Mtl_dt <- freq_descripteurs(Immigration_Montreal_seul, 
                                 cols = c("Vedettes"),
                                 combine = TRUE)

sujets_Immigr_Mtl_dt <- vedettes_Immigr_Mtl_dt[!descripteur %in% personnage_Immigr_Mtl_dt$descripteur]


# Régions seules
personnage_immigr_region_dt <- freq_descripteurs(Immigration_regions, 
                                           cols = c("Categorie_personnage"),
                                           combine = TRUE)

vedettes_immigr_region_dt <- freq_descripteurs(Immigration_regions, 
                                            cols = c("Vedettes"),
                                            combine = TRUE)

sujets_immigr_region_dt <- vedettes_immigr_region_dt[!descripteur %in% personnage_immigr_region_dt$descripteur]



# # Montréal mixte
# personnage_immigr_Mtl_mixte_dt <- freq_descripteurs(Immigration_Montreal_mixte,
#                                            cols = c("Categorie_personnage"),
#                                            combine = TRUE)
# 
# vedettes_immigr_Mtl_mixte_dt <- freq_descripteurs(Immigration_Montreal_mixte,
#                                                     cols = c("Vedettes"),
#                                                     combine = TRUE)
# 
# 
# sujets_immigr_Mtl_mixte_dt <- vedettes_immigr_Mtl_mixte_dt[!descripteur %in% personnage_immigr_Mtl_mixte_dt$descripteur]


# Tables pour Word

# Montréal seul
gt(data = personnage_Immigr_Mtl_dt[1:12]) |> 
  tab_header(
    title = "Roman montréalais de l'immigration (120)",
    subtitle = "Principaux types de personnages"
  ) |> 
  cols_label(descripteur = md("**Descripteur**"),
             "freq" = md("**Fréquence**")
  ) |> 
  tab_source_note(source_note = "Source: Romans à lire, BAnQ")

gt(data = sujets_Immigr_Mtl_dt[1:12]) |> 
  tab_header(
    title = "Roman montréalais de l'immigration (120)",
    subtitle = "Principaux sujets"
  ) |> 
  cols_label(descripteur = md("**Descripteur**"),
             "freq" = md("**Fréquence**")
  ) |> 
  tab_source_note(source_note = "Source: Romans à lire, BAnQ")


# Région seule

gt(data = personnage_immigr_region_dt[1:12]) |> 
  tab_header(
    title = "Roman régional de l'immigration (156)",
    subtitle = "Principaux types de personnages"
  ) |> 
  cols_label(descripteur = md("**Descripteur**"),
             "freq" = md("**Fréquence**")
  ) |> 
  tab_source_note(source_note = "Source: Romans à lire, BAnQ")

gt(data = sujets_immigr_region_dt[1:12]) |> 
  tab_header(
    title = "Roman régional de l'immigration (156)",
    subtitle = "Principaux sujets"
  ) |> 
  cols_label(descripteur = md("**Descripteur**"),
             "freq" = md("**Fréquence**")
  ) |> 
  tab_source_note(source_note = "Source: Romans à lire, BAnQ")



#### Tests statistiques -------------------------

############################################
## Romans dont le seul lieu est Montréal ##
############################################

ralMtl_stat <- copy(ralQcMtlSeul[, .(Id, Auteur, Titre, Vedettes,
                               Categorie_personnage, Lieu_geographique)])

# --------------------------------------------------- fonctions utilitaires

has_term <- function(col, pat) stringr::str_detect(col, pat)

# Immigration
ralMtl_stat[, Immigration_Diversite := as.integer(
  has_term(Vedettes, regex_immigration_diversite) |
    has_term(Categorie_personnage, regex_immigration_diversite) |
    has_term(Lieu_geographique, regex_immigration_diversite)
)]

# Alimentation
ralMtl_stat[, Alimentation := as.integer(
  has_term(Vedettes, pattern_alimentation) |
    has_term(Categorie_personnage, pattern_alimentation) |
    has_term(Lieu_geographique, pattern_alimentation)
)]


# Ne garder que les colonnes utiles
ralMtl_stat <- ralMtl_stat[, .(Id, Immigration_Diversite, Alimentation)]

## 4. Test d'exclusivité (XOR) ------------------------------------
romans_exclusive <- ralMtl_stat[
  (Immigration_Diversite + Alimentation) > 0]   # exclusif

table_exclusive <- table(romans_exclusive$Immigration_Diversite,
                         romans_exclusive$Alimentation)

print(table_exclusive)

## 5. Test statistique -------------------------------------------
if (any(table_exclusive < 5)){
  test_res <- fisher.test(table_exclusive)
} else {
  test_res <- chisq.test(table_exclusive)
}
print(test_res)

## 6. Proportions (%) --------------------------------------------
prop_pct <- round(prop.table(table_exclusive, margin = 1) * 100, 2)
print(prop_pct)



############################################
## Romans dont Montréal n'est pas un lieu ##
############################################

ralNonMtl_stat <- copy(ralQcNonMtl[, .(Id, Auteur, Titre, Vedettes,
                               Categorie_personnage, Lieu_geographique)])


# Alimentation (définir regex_alimentation au préalable)
ralNonMtl_stat[, Alimentation := as.integer(
  has_term(Vedettes, pattern_alimentation) |
    has_term(Categorie_personnage, pattern_alimentation) |
    has_term(Lieu_geographique, pattern_alimentation)
)]

# Immigration / Diversité
ralNonMtl_stat[, Immigration_Diversite := as.integer(
  has_term(Vedettes, regex_immigration_diversite) |
    has_term(Categorie_personnage, regex_immigration_diversite) |
    has_term(Lieu_geographique, regex_immigration_diversite)
)]

# Ne garder que les colonnes utiles
ralNonMtl_stat <- ralNonMtl_stat[, .(Id, Immigration_Diversite, Alimentation)]

# 4. Test d'exclusivité (XOR) ------------------------------------
romans_exclusive <- ralNonMtl_stat[
  (Immigration_Diversite + Alimentation) > 0]   # exclusif

table_exclusive <- table(romans_exclusive$Immigration_Diversite,
                         romans_exclusive$Alimentation)

print(table_exclusive)

# 5. Test statistique -------------------------------------------
if (any(table_exclusive < 5)){
  test_res <- fisher.test(table_exclusive)
} else {
  test_res <- chisq.test(table_exclusive)
}
print(test_res)

# 6. Proportions (%) --------------------------------------------
prop_pct <- round(prop.table(table_exclusive, margin = 1) * 100, 2)
print(prop_pct)


####################################################
## Romans qui situe notamment l'action à Montréal ##
####################################################

ralQcMtl_stats <- data.table::copy(ralQcMtl_Mixte[, .(Id, Auteur, Titre, Vedettes, Categorie_personnage, Lieu_geographique)])


# Alimentation (définir regex_alimentation au préalable)
ralQcMtl_stats[, Alimentation := as.integer(
  has_term(Vedettes, pattern_alimentation) |
    has_term(Categorie_personnage, pattern_alimentation) |
    has_term(Lieu_geographique, pattern_alimentation)
)]

# Immigration / Diversité
ralQcMtl_stats[, Immigration_Diversite := as.integer(
  has_term(Vedettes, regex_immigration_diversite) |
    has_term(Categorie_personnage, regex_immigration_diversite) |
    has_term(Lieu_geographique, regex_immigration_diversite)
)]

# Ne garder que les colonnes utiles
ralQcMtl_stats <- ralQcMtl_stats[, .(Id, Immigration_Diversite, Alimentation)]

## 4. Test d'exclusivité (XOR) ------------------------------------
romans_exclusive <- ralQcMtl_stats[
  (Immigration_Diversite + Alimentation) > 0]   # exclusif

table_exclusive <- table(romans_exclusive$Immigration_Diversite,
                         romans_exclusive$Alimentation)

print(table_exclusive)

## 5. Test statistique -------------------------------------------
if (any(table_exclusive < 5)){
  test_res <- fisher.test(table_exclusive)
} else {
  test_res <- chisq.test(table_exclusive)
}
print(test_res)

## 6. Proportions (%) --------------------------------------------
prop_pct <- round(prop.table(table_exclusive, margin = 1) * 100, 2)
print(prop_pct)


