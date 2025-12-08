#################################
#### TRAITEMENT DES DONNÉES #####
#################################

# Nombre d'inscriptions par faculté
library(data.table)
library(stringr)

### Importation RAL version août 2025 (81k notices) --- 
ral <- fread("donnees/romansalire_202508.csv")

# --- Renommer colonnes ---
setnames(
  ral,
  old = c(
    "Année de publication",
    "VM – Nom de personne",
    "VM – Nom de collectivité",
    "VM – Nom de réunion",
    "VM – Terme chronologique",
    "VM – Nom commun",
    "VM – Nom géographique",
    "Genre littéraire",
    "Littérature nationale",
    "VM – Catégorie de personne",
    "Numéro de séquence",
    "Éditeur",
    "Lieu de publication",
    "Pays de publication",
    "Prix littéraire du Québec"
  ),
  new = c(
    "Annee_publication",
    "Nom_de_personne",
    "Nom_de_collectivite",
    "Nom_de_reunion",
    "Periode_chronologique",
    "Vedettes",
    "Lieu_geographique",
    "Genre_litteraire",
    "Litterature_nationale",
    "Categorie_personnage",
    "Id",
    "Editeur",
    "Lieu_de_publication",
    "Pays_de_publication",
    "Prix_litteraire_qc"
  )
)

# --- Suppressions regroupées ---
cols_to_remove <- c(
  "Description matérielle",
  "Adaptation",
  "Prix",
  "VM – Titre uniforme",
  "Édition"
)

ral[, (cols_to_remove) := NULL]

#### Élimination des titres en anglais et des doublons ----

ral <- ral[Langue == "fre"]
ral <- ral[!duplicated(ral[, .(Auteur, Titre)])]
ral <- ral[!duplicated(ral[, .(Titre, Vedettes)])] 

#### Création de variables catégorielles ----
ral[, Pays_cat := fcase(
  str_detect(Pays_de_publication, "fr"),  "France",
  str_detect(Pays_de_publication, "quc"), "Québec",
  default = "Autre"
)]


#### Traitement des années de publication ----

ral[str_detect(Annee_publication, "[0-9]", negate = TRUE), .(Auteur, Titre, Editeur, Annee_publication)] # 3 titres


ral[Auteur == "Chevalier, H. Émile (Henri Émile), 1828-1879" &
      Titre == "L'Île de Sable", Annee_publication:="1854"]

ral[Auteur == "Constantin-Weyer, Maurice, 1881-1964" &
      Titre == "Un Sourire dans la tempête", Annee_publication:="1934"]

ral[Auteur == "Depierre, Marie-Ange, 1951-" &
      Titre == "Une Petite liberté : récits ; suivi de, Dire oui à Clarice Lispector", Annee_publication:="1989"]

ral[Auteur == "Carducci, Lisa, 1943-" &
      Titre == "Stagioni d'Amore", Annee_publication:="1997"]


ral[, Annee_publication:=as.integer(Annee_publication)]



#### Création de sous-corpus ----
ralQc <- ral[Pays_cat == "Québec"]

regex_mtl_strict <- "^([^;]*Montréal[^;]*)(;\\s*[^;]*Montréal[^;]*)*$"

ralQcMtlSeul <- ralQc[str_detect(Lieu_geographique, 
                                 regex(regex_mtl_strict, ignore_case = TRUE)) &
                        between(Annee_publication, left = 1980, right = 2024)]

# Motif "Strict Québec"
# Chaque segment (séparé par ;) doit contenir le mot "Québec"
regex_quebec_strict <- "^([^;]*Québec[^;]*)(;\\s*[^;]*Québec[^;]*)*$"

# 2. Appliquer le double filtre
ralQcNonMtl <- ral[
  Pays_cat == "Québec" &
    # EXCLURE Montréal (condition absolue)
    str_detect(Lieu_geographique, regex("Montréal", ignore_case = TRUE), negate = TRUE) &
    # INCLURE uniquement si TOUS les lieux sont au Québec
    str_detect(Lieu_geographique, regex(regex_quebec_strict, ignore_case = TRUE)) &
    between(Annee_publication, left = 1980, right = 2024)
]


# Corpus Mixte
ralQcMtl_Mixte <- ral[
  Pays_cat == "Québec" &
    # Il FAUT qu'il y ait Montréal quelque part
    str_detect(Lieu_geographique, regex("Montréal", ignore_case = TRUE)) &
    # MAIS ce ne doit PAS être un "Montréal Strict"
    str_detect(Lieu_geographique, regex(regex_mtl_strict, ignore_case = TRUE), negate = TRUE)&
    between(Annee_publication, left = 1980, right = 2024)
]


# ralFr <- ral[Pays_cat == "France", .(Id, Auteur, Titre, Editeur, Annee_publication, Vedettes, Lieu_geographique, Categorie_personnage)]
# 
# ralParisSeul <- ralFr[str_detect(Lieu_geographique,
#                                  regex("^Paris \\(France\\)$", ignore_case = FALSE))]
# 
# ralFrNonParis <- ralFr[str_detect(Lieu_geographique,
#                                   regex("Paris", ignore_case = FALSE),
#                                   negate = TRUE) &
#                          !Lieu_geographique == "" &
#                          str_detect(Lieu_geographique, "France")]
# 
# ralParis <- ral[str_detect(Lieu_geographique,
#                            regex("Paris", ignore_case = FALSE)) &
#                   !Lieu_geographique == ""]

if(!dir.exists("donnees_traitees")) {dir.create("donnees_traitees")}
# saveRDS(ral, "donnees_traitees/donneesAll_ready2use.RDS")
saveRDS(ralQc, "donnees_traitees/ralQc.RDS")
saveRDS(ralQcMtlSeul, "donnees_traitees/ralQcMtlSeul.RDS")
saveRDS(ralQcNonMtl, "donnees_traitees/ralQcNonMtl.RDS")
saveRDS(ralQcMtl_Mixte, "donnees_traitees/ralQcMtl.RDS")
# saveRDS(ralFr, "donnees_traitees/ralFr.RDS")
# saveRDS(ralParisSeul, "donnees_traitees/ralParisSeul.RDS")
# saveRDS(ralFrNonParis, "donnees_traitees/ralFrNonParis.RDS")
# saveRDS(ralParis, "donnees_traitees/ralParis.RDS")






