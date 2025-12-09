#### Fonctions pour analyse ----

library(data.table)
library(stringi) 
# library(wordcloud2) 

# ------------------------------------------------------------
#' Fréquence des descripteurs issus de plusieurs colonnes
#'
#' @param dt          data.table contenant les colonnes à analyser
#' @param cols        vecteur de noms de colonnes à exploiter
#' @param sep         séparateur utilisé dans les chaînes (défaut ";")
#' @param rm_paren    logique : TRUE → suppression du texte entre parenthèses
#' @param exclude     vecteur de descripteurs à exclure (exact match, case‑insensitive)
#' @param combine     logique : TRUE → fusionner les colonnes en une seule fréquence
#'                    FALSE → retourner une liste de tables, une par colonne
#' @return            data.table (ou liste de data.tables) avec deux colonnes :
#'                    `descripteur` et `freq`
#' ------------------------------------------------------------
freq_descripteurs <- function(dt,
                              cols = c("Vedettes",
                                       "Categorie_personnage",
                                       "Lieu_geographique"),
                              sep = ";",
                              rm_paren = TRUE,
                              exclude = NULL,
                              combine = TRUE) {
  
  #--- 1. Vérifications de base -------------------------------------------------
  stopifnot(is.data.table(dt))
  miss <- setdiff(cols, names(dt))
  if (length(miss) > 0L)
    stop("Colonnes manquantes dans 'dt' : ", paste(miss, collapse = ", "))
  
  #--- 2. Fonction interne de nettoyage -----------------------------------------
  clean_one <- function(x) {
    # x : vecteur de chaînes (une colonne)
    # 2.1. Éclater la chaîne
    splitted <- strsplit(x, split = sep, fixed = TRUE)
    
    # 2.2. Aplatir + retirer les NA/"" éventuels
    tokens <- unlist(splitted, use.names = FALSE)
    tokens <- tokens[tokens != "" & !is.na(tokens)]
    
    # 2.3. Suppression des parenthèses (et du texte qu’elles contiennent)
    if (rm_paren) {
      # on enlève tout ce qui est entre parenthèses, y compris l’espace qui les précède
      tokens <- gsub("\\s*\\([^\\)]*\\)|--.+", "", tokens, perl = TRUE)
    }
    
    # 2.4. Trim (suppression des espaces en tête/tail)
    tokens <- stri_trim_both(tokens)
    
    # 2.5. Normaliser la casse (facultatif, on garde la casse d'origine mais on
    #      rend l'exclusion insensible à la casse)
    tokens
  }
  
  #--- 3. Traitement colonne par colonne ----------------------------------------
  result_list <- lapply(cols, function(col) {
    dt[[col]] |> clean_one()
  })
  names(result_list) <- cols
  
  #--- 4. Exclusion éventuelle --------------------------------------------------
  if (!is.null(exclude) && length(exclude) > 0L) {
    # on compare en ignorant la casse et les espaces superflus
    excl_norm <- tolower(stri_trim_both(exclude))
    result_list <- lapply(result_list, function(v) {
      v[!tolower(v) %in% excl_norm]
    })
  }
  
  #--- 5. Construction de la table de fréquence ---------------------------------
  if (combine) {
    # on fusionne toutes les colonnes
    all_tokens <- unlist(result_list, use.names = FALSE)
    freq_dt <- data.table(descripteur = all_tokens)[,
                                                    .(freq = .N), by = descripteur][order(-freq)]
    return(freq_dt)
  } else {
    # on renvoie une liste de data.tables, une par colonne
    lapply(result_list, function(v) {
      data.table(descripteur = v)[,
                                  .(freq = .N), by = descripteur][order(-freq)]
    })
  }
}






# ------------------------------------------------------------
#' Crée un nuage de mots à partir d'une table de fréquence
#'
#' @param freq_dt   data.table ou liste de data.tables contenant
#'                  les colonnes `descripteur` et `freq`.
#' @param engine    moteur de rendu : "wordcloud2" (défaut) ou "ggwordcloud".
#' @param min_freq  fréquence minimale à afficher (exclut les plus rares).
#' @param max_words nombre maximal de mots affichés (triés par fréquence).
#' @param shape     forme du nuage (pour wordcloud2) : "circle", "star", …
#' @param palette   vecteur de couleurs (ou nom d’une palette RColorBrewer).
#' @param size      facteur d'agrandissement du nuage (wordcloud2) ou
#'                  multiplicateur de la taille des mots (ggwordcloud).
#' @param random_order  logique : TRUE → placer les mots dans l'ordre aléatoire,
#'                  FALSE → placer du plus fréquent au centre (défaut FALSE).
#' @param ...       arguments supplémentaires transmis au moteur choisi.
#' @return          L'objet graphique produit par le moteur sélectionné.
#' ------------------------------------------------------------


nuage_de_mots <- function(freq_dt,
                          engine = c("wordcloud2", "ggwordcloud"),
                          min_freq = 1,
                          max_words = Inf,
                          shape = "circle",
                          palette = "Dark2",
                          size = 1,
                          random_order = FALSE,
                          ...) {

  engine <- match.arg(engine)

  # Normaliser l'entrée (data.table ou liste de data.tables)
  if (is.list(freq_dt) && !inherits(freq_dt, "data.table")) {
    dt <- data.table::rbindlist(freq_dt, use.names = TRUE, fill = TRUE, idcol = "source_col")
  } else if (inherits(freq_dt, "data.table")) {
    dt <- data.table::copy(freq_dt)
  } else if (is.data.frame(freq_dt)) {
    dt <- data.table::as.data.table(freq_dt)
  } else {
    stop("`freq_dt` doit être une data.table, une data.frame, ou une liste de telles tables.")
  }

  # Vérifier colonnes attendues
  if (!all(c("descripteur", "freq") %in% names(dt))) {
    stop("`freq_dt` doit contenir les colonnes `descripteur` et `freq`.")
  }

  # s'assurer que freq est numérique
  dt[, freq := as.numeric(freq)]

  # Filtrage
  dt <- dt[freq >= min_freq]
  data.table::setorder(dt, -freq)
  if (is.finite(max_words)) dt <- dt[seq_len(min(.N, max_words))]

  # Palette -> vecteur de couleurs
  cols <- NULL
  if (is.character(palette) && length(palette) == 1L &&
      palette %in% rownames(RColorBrewer::brewer.pal.info)) {
    ncol_needed <- max(1, min(8, dt[,.N]))
    cols <- RColorBrewer::brewer.pal(ncol_needed, palette)
  } else if (is.character(palette)) {
    cols <- palette
  } else {
    stop("`palette` doit être un nom de palette RColorBrewer ou un vecteur de couleurs.")
  }
  cols <- rep(cols, length.out = nrow(dt))

  # MOTEUR wordcloud2
  if (engine == "wordcloud2") {
    # wordcloud2 veut des colonnes 'word' et 'freq'
    df_wc <- as.data.frame(dt[, .(word = descripteur, freq = freq)])
    # charger la lib au moment de l'appel (si pas déjà chargée)
    if (!requireNamespace("wordcloud2", quietly = TRUE)) {
      stop("Le package 'wordcloud2' n'est pas installé. Installez-le ou utilisez engine = 'ggwordcloud'.")
    }
    wc <- wordcloud2::wordcloud2(
      data = df_wc,
      size = size,
      color = cols,
      shape = shape,
      backgroundColor = "white",
      rotateRatio = 0.1,
      minRotation = -pi/6,
      maxRotation = pi/6,
      shuffle = random_order,    # <-- correction importante
      ...
    )
    return(wc)
  }

  # MOTEUR ggwordcloud
  if (!requireNamespace("ggwordcloud", quietly = TRUE) || !requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Les packages 'ggplot2' et 'ggwordcloud' sont requis pour engine = 'ggwordcloud'.")
  }

  dt_for_gg <- as.data.frame(dt[, .(descripteur, freq)])
  gg <- ggplot2::ggplot(dt_for_gg,
                        ggplot2::aes(label = descripteur,
                                     size = freq * size,
                                     colour = factor(seq_len(nrow(dt_for_gg))))) +
    ggwordcloud::geom_text_wordcloud(area_corr = TRUE, random_order = random_order, ...) +
    ggplot2::scale_size_area(max_size = 20) +
    ggplot2::scale_colour_manual(values = rep(cols, length.out = nrow(dt_for_gg))) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "none",
                   plot.background = ggplot2::element_rect(fill = "white", colour = NA))

  return(gg)
}


# Fonctions pour le resampling et l'équilibrage des ensembles
perform_resampling <- function(data, num_resamples = 1000) {
  chi_squared_stats <- numeric(num_resamples)
  p_values <- numeric(num_resamples)

  # Boucle pour le resampling
  for (i in 1:num_resamples) {
    # Échantillonnage des non-Montréal
    non_montreal_sample <- data[Mtl == 0][sample(.N, 1329)]

    # Fusion avec les données Montréal
    sample_data <- rbind(data[Mtl == 1], non_montreal_sample)

    # Table de contingence
    contingency_table <- table(sample_data$Mtl, sample_data$Diversite)

    # Test du chi-carré
    test_result <- chisq.test(contingency_table)
    chi_squared_stats[i] <- test_result$statistic
    p_values[i] <- test_result$p.value
  }

  # Renvoyer les résultats moyens
  list(mean_chi_squared = mean(chi_squared_stats), mean_p_value = mean(p_values))
}


# Même fonction, pour le test exact de Fisher
perform_resampling_fisher <- function(data, times = 1000) {
  set.seed(123)  # Pour la reproductibilité

  p_values <- numeric(times)
  odds_ratios <- numeric(times)

  # Nombre de romans à Montréal et hors Montréal
  n_montreal <- sum(data$Mtl == 1)
  n_non_montreal <- sum(data$Mtl == 0)

  # Taille de l'échantillon équilibré
  min_size <- min(n_montreal, n_non_montreal)

  for (i in 1:times) {
    # Échantillonnage des données de Montréal et non Montréal
    sample_montreal <- data[data$Mtl == 1][sample(.N, min_size, replace = TRUE)]
    sample_non_montreal <- data[data$Mtl == 0][sample(.N, min_size, replace = TRUE)]

    # Fusionner les échantillons resamplés
    sample_data <- rbind(sample_montreal, sample_non_montreal)

    # Exécuter le test exact de Fisher
    test_result <- fisher.test(table(sample_data$Mtl, sample_data$Diversite))

    # Stocker la valeur-p et le rapport de cotes
    p_values[i] <- test_result$p.value
    odds_ratios[i] <- test_result$estimate
  }

  # Retourner les moyennes des valeurs-p et des rapports de cotes
  list(mean_p_value = mean(p_values), mean_odds_ratio = mean(odds_ratios))
}
