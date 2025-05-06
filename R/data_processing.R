# LISTE DES FONCTIONS DE TRAITEMENT DE DONNEES

## Chargement des librairies
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

## Chargement de fichier .csv
load_results <- function(file_path) {
  read_csv(file_path)
}

## Chargement de fichier .asc
read_asc <- function(file_path) {
  read.csv(file_path, skip = 20, header = FALSE, sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
}

## Fonction pour extraire la date du fichier .asc
extract_and_format_date <- function(file_name) {
  dmy(str_extract(file_name, "\\d{2}-\\d{2}-\\d{4}")) %>% format("%Y-%m-%d")
}

## Fonction de filtre par athlète sélectionné
format_data_by_athlete <- function(results, athlete_name) {
  results %>%
    mutate(DateFormatted = extract_and_format_date(File)) %>%
    arrange(DateFormatted) %>%
    group_by(Stage) %>%
    mutate(Nuit = row_number()) %>%
    ungroup()
}

## Fonction pour nettoyer les données brutes .asc
process_row_data <- function(data, spo2_max, spo2_min, seuil) {
  data %>%
    select(-V1) %>%
    rename(annee = V2, mois = V3, jour = V4, heure = V5, minute = V6, seconde = V7, pouls = V8, spo2 = V9) %>%
    mutate(date = make_datetime(annee, mois, jour, heure, minute, seconde)) %>%
    select(date, spo2) %>%
    separate(date, into = c("date", "heure"), sep = " ") %>%
    filter(spo2 < spo2_max, spo2 > spo2_min) %>%
    mutate(heure = ymd_hms(paste(date, heure),truncated = 3),
           num = row_number(),
           duree_cumulee = num * 4,
           sous_seuil = spo2 < seuil) %>%
    group_by(grp = cumsum(c(FALSE, diff(sous_seuil) != 0))) %>%
    ungroup()
}

## Fonction pour calculer les charges hypoxiques (HB, REDTA)
calculate_burden <- function(donnees, seuil) {
  duree_totale <- tail(donnees$duree_cumulee, 1)

  data_sub_seuil <- donnees %>% filter(sous_seuil)

  data_sub_seuil_aire <- data_sub_seuil %>%
    group_by(grp) %>%
    summarise(aire_sous_seuil = AUC(as.numeric(heure), abs(spo2 - seuil), na.rm = TRUE)) %>%
    replace_na(list(aire_sous_seuil = 0))

  HB <- 60 * sum(data_sub_seuil_aire$aire_sous_seuil) / duree_totale
  REDTA <- sum(data_sub_seuil_aire$aire_sous_seuil) / 3600

  list(HB = HB, REDTA = REDTA, duree_totale = duree_totale)
}

## Fonction pour traiter un fichier .asc
process_file <- function(file_path, seuil, spo2_max, spo2_min) {
  data <- tryCatch({
    read_asc(file_path) %>%
      lapply(function(x) if (is.character(x)) iconv(x, from = "UTF-8", to = "UTF-8", sub = "") else x) %>%
      as.data.frame()
  }, error = function(e) {
    cat("Erreur lors de la lecture du fichier :", file_path, "\n", e$message, "\n")
    return(NULL)
  })

  if (is.null(data)) return(NULL)

  donnees <- tryCatch({
    process_row_data(data, spo2_max, spo2_min, seuil)
  }, error = function(e) {
    cat("Erreur lors du traitement des données dans le fichier :", file_path, "\n", e$message, "\n")
    return(NULL)
  })

  if (is.null(donnees)) return(NULL)

  calculate_burden(donnees, seuil)
}

## Fonction pour calculer le HB et REDTA d'un athlète donné, pour un seuil
process_athlete_data <- function(athlete_name, root_dir, seuil, spo2_max = 95, spo2_min = 85) {
  results <- list()
  athlete_folder <- file.path(root_dir, athlete_name)

  if (!file.exists(athlete_folder)) {
    cat("Le dossier pour l'athlète", athlete_name, "n'existe pas.\n")
    return(NULL)
  }

  stages <- list.dirs(athlete_folder, recursive = FALSE)

  for (stage in stages) {
    stage_name <- basename(stage)
    asc_files <- list.files(stage, pattern = "\\.asc$", full.names = TRUE)

    for (file in asc_files) {
      cat("Processing:", athlete_name, "-", stage_name, "-", basename(file), "\n")
      result <- process_file(file, seuil, spo2_max, spo2_min)

      if (is.null(result)) {
        cat("Erreur dans ce fichier :", file, "- Ignoré.\n")
        next
      }

      results <- append(results, list(list(athlete = athlete_name, stage = stage_name, file = basename(file), result = result)))
    }
  }
  
  results_df <- do.call(rbind, lapply(results, function(x) {
    data.frame(
      Athlete = x$athlete,
      Stage = x$stage,
      File = x$file,
      HB = x$result$HB,
      REDTA = x$result$REDTA,
      DureeTotale = x$result$duree_totale
    )
  }))

  format_data_by_athlete(results_df, athlete_name)
}
