#### LISTE DES FONCTIONS DE TRAITEMENT DES DONNÉES ####

##### CHARGEMENT DES LIBRAIRIES ====
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
#library(roxygen2)


##### Lecture de fichiers .asc ====
#' Permet de lire un fichier .asc, et de retirer les mentions inutiles
#' 
#' @param file_path chemin relatif du fichier
#' @return tableau de données structurées

read_asc <- function(file_path) {
  read.csv(file_path, skip = 20, header = FALSE, sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
}

##### Extraction de date d'un fichier ====
#' Permet d'extraire la date à partir du nom du fichier
#' 
#' @param file_name nom du fichier
#' @return date au format YYYY-MM-JJ

extract_and_format_date <- function(file_name) {
  dmy(str_extract(file_name, "\\d{2}-\\d{2}-\\d{4}")) %>% format("%Y-%m-%d")
}

##### Traitement des fichiers .asc ====
#' Permet de traiter les fichiers .asc et de rendre exploitable les données
#' 
#' @param data set de données issue de la fonction read_asc
#' @param spo2_max valeur haute limite des données acceptables
#' @param spo2_min valeur basse limite des données acceptables
#' @param seuil valeur du seuil de désaturation
#' @return tibble contenant les données de SpO2 pour chaque points de mesures. 
#' Les valeurs sont filtrées en fonction des seuils définis

process_row_data <- function(data, spo2_max, spo2_min, seuil) {
  data %>%
    #Retrait de la première colonne
    select(-V1) %>%
    #Renommage des colonnes
    rename(annee = V2, mois = V3, jour = V4, heure = V5, minute = V6, seconde = V7, pouls = V8, spo2 = V9) %>%
    #Formatage de la date
    mutate(date = make_datetime(annee, mois, jour, heure, minute, seconde)) %>%
    #Sélection des colonnes date et spo2
    select(date, spo2) %>%
    #Différenciation de l'heure et de la date
    separate(date, into = c("date", "heure"), sep = " ") %>%
    #Filtrage des valeurs inférieur et supérieur aux bornes spo2
    filter(spo2 < spo2_max, spo2 > spo2_min) %>%
    # ajout des colonnes heure, num, duree_cumulee et sous_seuil
    mutate(heure = ymd_hms(paste(date, heure),truncated = 3),
           num = row_number(),
           duree_cumulee = num * 4, # 4 = fréquence de mesures (toutes les 4sec)
           sous_seuil = spo2 < seuil) %>%
    #Groupement des séquences
    group_by(grp = cumsum(c(FALSE, diff(sous_seuil) != 0))) %>%
    ungroup()
}

##### Calcul de la charge hypoxique ====
#' Permet de calculer les valeurs Hypoxic Burden (HB) et Respiratory Event Desaturation Transient Area (REDTA) 
#' 
#' @param donnees set de données issue de la fonction read_asc
#' @param seuil valeur du seuil de désaturation
#' @return $HB, $REDTA et $duree_totale pour une nuit donnée
#' 

calculate_burden <- function(donnees, seuil) {
  #Calucl de la durée totale d'enregistrement (dernière valeure de mesure)
  duree_totale <- tail(donnees$duree_cumulee, 1)
  
  #Création d'un tibble filtré sur les valeurs sous-seuil
  data_sub_seuil_aire <- donnees %>%
    filter(sous_seuil) %>% 
    group_by(grp) %>%
    #Calcul de l'aire sous la courbe pour chaque groupe
    summarise(aire_sous_seuil = AUC(as.numeric(heure), abs(spo2 - seuil), na.rm = TRUE)) %>%
    replace_na(list(aire_sous_seuil = 0))
  
    #Calcul des valeurs HB et REDTA selon publication 
    # A Review of Novel Oximetry Parameters for the Prediction of Cardiovascular Disease in Obstructive Sleep Apnoea
    # Siying He et al. (2023)
  HB <- 60 * sum(data_sub_seuil_aire$aire_sous_seuil) / duree_totale
  REDTA <- sum(data_sub_seuil_aire$aire_sous_seuil) / 3600

  list(HB = HB, REDTA = REDTA, duree_totale = duree_totale)
}


##### Extraction du nombre de nuit par stage ====
#' Permet d'extraire le nombre de nuit et de les grouper par stage
#' 
#' @param results un dataframe contenant l'évolution de la charge hypoxique
#' @return un tibble results avec en plus le numéro de nuit, et la date extraite

format_data_by_athlete <- function(results) {
  results <- results %>%
    # Extraction de la date du nom du fichier
    mutate(Date = extract_and_format_date(File)) %>%
    # Ordre chronologique
    arrange(Date) %>%
    # Regroupement par stage
    group_by(Stage) %>%
    # Une nuit = un numéro par ordre chronologique
    mutate(Nuit = row_number()) %>%
    ungroup()
  
  results
}


##### Calcul de l'évolution de la charge hypoxique ====
#' Permet de calculer l'évolution de la charge hypoxique d'un athlète au cours d'un stage
#' 
#' @param athlete_name nom de l'ahtlète à traiter
#' @param root_dir dossier de traitement des données
#' @param seuil valeur du seuil de désaturation
#' @param spo2_max valeur haute limite des données acceptables
#' @param spo2_min valeur basse limite des données acceptables
#' @return tibble contenant les veleurs HB/REDTA/duree_totale pour chaque nuit de chaque stage d'un athlète

process_athlete_data <- function(athlete_name, root_dir, seuil, spo2_max, spo2_min) {
  results_final <- list()
  #Création du chemin relatif du dossier de l'athlète
  athlete_folder <- file.path(root_dir, athlete_name)

  # Listing des stages d'un athlète
  stages <- list.dirs(athlete_folder, recursive = FALSE)

  # Pour chaque stage, lister les fichiers .asc
  for (stage in stages) {
    stage_name <- basename(stage)
    asc_files <- list.files(stage, pattern = "\\.asc$", full.names = TRUE)
  
    #Pour chaque fichier .asc présents dans un dossier d'athlète: 
    for (file in asc_files) {
      cat("Processing:", athlete_name, "-", stage_name, "-", basename(file), "\n")
      # Utilisation de la fonction read_asc
      data_file <- read_asc(file)
      # Traitement des données brutes du fichier
      result <- process_row_data(data_file, spo2_max, spo2_min, seuil)
      # Calcul de la charge hypoxique (HB/REDTA)
      result_hypox <- calculate_burden(result, seuil)
      
      #Gestion des exceptions pour les fichiers corrompus
      if (is.null(result_hypox)) {
        cat("Erreur dans ce fichier :", file, "- Ignoré.\n")
        next
      }
      
      # Création d'une liste de liste contenant les résultats
      results_final <- append(results_final, list(
        list(
          athlete = athlete_name,
          stage = stage_name,
          file = basename(file),
          HB = result_hypox$HB,
          REDTA = result_hypox$REDTA,
          duree_totale = result_hypox$duree_totale
      )))
      
    }
  }
  
  # Transformation de la liste de liste en tibble
  results_df <- bind_rows(lapply(results_final, function(x) {
      data.frame(
        Athlete = x$athlete,
        Stage = x$stage,
        File = x$file,
        HB = x$HB,
        REDTA = x$REDTA,
        DureeTotale = x$duree_totale,
        stringsAsFactors = FALSE
      )
    }))
  
  # Mutation du tibble pour lister les nuits par stage
  format_data_by_athlete(results_df)
}


