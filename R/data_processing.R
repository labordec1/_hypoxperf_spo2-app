# LISTE DES FONCTIONS DE TRAITEMENT DE DONNEES

## Chargement des librairies
library(readr)
library(dplyr)
library(lubridate)


## Chargement de fichier .csv
load_results <- function(file_path) {
  results <- read_csv(file_path)
  return(results)
}

## Chargement de fichier .asc
read_asc <- function(file_path) {
  data <- read.csv(file_path, skip = 20, header = FALSE, sep = ",", stringsAsFactors = FALSE)
  return(data)
}


## Fonction pour extraire la date du fichier .asc
extract_and_format_date <- function(file_name) {
  date_part <- str_extract(file_name, "\\d{2}-\\d{2}-\\d{4}")
  date_obj <- dmy(date_part)
  formatted_date <- format(date_obj, "%Y-%m-%d")
  return(formatted_date)
}


## Fonction de filtre par athlète sélectionné
filter_results_by_athlete <- function(results, athlete_name) {
  results_filtered <- results %>%
    mutate(DateFormatted = extract_and_format_date(File)) %>%
    filter(Athlete == athlete_name) %>%
    arrange(DateFormatted) %>%
    group_by(Stage) %>%
    mutate(Count = row_number()) %>%
    ungroup()
  
  return(results_filtered)
}


## Fonction pour nettoyer les données bruts .asc
process_row_data <- function(data, spo2_max, spo2_min, seuil) {
  donnees <- data %>%
    select(-V1) %>%
    rename(annee = V2,
           mois = V3,
           jour = V4,
           heure = V5,
           minute = V6,
           seconde = V7,
           pouls = V8,
           spo2 = V9) %>%
    mutate(date = make_datetime(annee, mois, jour, heure, minute, seconde)) %>%
    select(date, spo2) %>%
    separate(date, into = c("date", "heure"), sep = " ") %>%
    filter(spo2 < spo2_max, spo2 > spo2_min) %>%
    mutate(heure = ymd_hms(paste(date, heure)))
  
  donnees <- donnees %>%
    mutate(num = row_number()) %>%
    mutate(duree_cumulee = num * 4) %>%
    mutate(sous_seuil = spo2 < seuil) %>%
    group_by(grp = cumsum(c(FALSE, diff(sous_seuil) != 0))) %>%
    ungroup()
  
  return(donnees)
}

## Fonction pour calculer les charges hypoxiques (HB, REDTA) tel que défini dans la littérature

calculate_burden <- function(donnees, seuil) {
  duree_totale <- tail(donnees$duree_cumulee, 1)
  
  data_sub_seuil <- donnees %>%
    filter(sous_seuil)
  
  data_sub_seuil_aire <- data_sub_seuil %>%
    group_by(grp) %>%
    summarise(aire_sous_seuil = AUC(as.numeric(heure), abs(spo2 - seuil), na.rm = TRUE)) %>%
    replace_na(list(aire_sous_seuil = 0))
  
  HB <- 60 * sum(data_sub_seuil_aire$aire_sous_seuil) / duree_totale
  REDTA <- sum(data_sub_seuil_aire$aire_sous_seuil) / 3600
  
  return(list(HB = HB, REDTA = REDTA))
}

