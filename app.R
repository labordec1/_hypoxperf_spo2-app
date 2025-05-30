# CHARGEMENT DES LIBRAIRIES
library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(DescTools)
library(zoo)
library(bslib)
library(plotly)

# DEFINITION DE L'ESPACE DE TRAVAIL
app_dir <- getwd()
root_dir <- file.path(app_dir, "data", "PROD")

# CHARGEMENT DES FONCTIONS
source("R/data_processing.R")
source("R/plotting.R")


# CHARGEMENT DU DATASET


## Liste des athlètes
athletes <- list.dirs(root_dir, recursive = FALSE)

# CREATION DE L'UI

ui <- page_navbar(
  title = "HYPOXPERF - SpO2",
  bg = "#2D89C8",
  inverse = TRUE,
  
  # Visualisation de la SpO2 par nuit, par athlète
  nav_panel(
    title = "Visualisation SpO2",
    sidebarLayout(
      sidebarPanel(
        selectInput("athlete", "Choisir un athlète:", choices = basename(athletes)),
        uiOutput("stageSelector"),
        uiOutput("fileSelector"),
        sliderInput("seuil", "Définir le seuil SpO2:", min = 85, max = 95, value = 92, step = 1) # Nouveau curseur
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Évolution de la SpO2 par nuit", plotlyOutput("dataPlot"))
        )
      )
    )
  ),
  
  # Visualisation des courbes d'évolution de HB et REDTA
  nav_panel(
    title = "Évolution Charge Hypoxique",
    sidebarLayout(
      sidebarPanel(
        selectInput("athlete_global", "Choisir un athlète:", choices = basename(athletes)),
        sliderInput("seuil_global", "Définir le seuil SpO2:", min = 85, max = 95, value = 92, step = 1), # Nouveau curseur
        downloadButton("downloadData", "Télécharger les données") # Bouton de téléchargement
      ),
      mainPanel(
        plotlyOutput("hbPlot"),
        plotlyOutput("redtaPlot")
      )
    )
  ),
  
  # Gestion des fichiers
  nav_panel(
    title = "Gestion des fichiers",
    sidebarLayout(
      sidebarPanel(
        textInput("new_athlete", "Créer un nouvel athlète (nom du dossier):"),
        selectInput("existing_athlete", "Sélectionner un athlète existant:", choices = basename(athletes)),
        textInput("new_stage", "Créer un nouveau stage (nom du dossier):"),
        fileInput("new_files", "Déposer des fichiers bruts (.asc):", multiple = TRUE, accept = c(".asc")),
        actionButton("submit_changes", "Appliquer les modifications")
      ),
      mainPanel(
        verbatimTextOutput("action_status") # Afficher le statut des actions
      )
    )
  )
)


# DEFINITION DE LA LOGIQUE SERVEUR

server <- function(input, output, session) {
  
## Partie "Visualisation SpO2"
  
  #Sélection du stage
  output$stageSelector <- renderUI({
    req(input$athlete)
    selected_athlete <- input$athlete
    stage_dirs <- list.dirs(file.path(root_dir, selected_athlete), recursive = FALSE) # nolint
    selectInput("stage", "Choisir un stage:", choices = basename(stage_dirs))
  })
  
  #Sélection de la nuit
  output$fileSelector <- renderUI({
    req(input$athlete, input$stage)
    selected_stage <- input$stage
    selected_athlete <- input$athlete
    files <- list.files(file.path(root_dir, selected_athlete, selected_stage), pattern = "\\.asc$", full.names = TRUE) # nolint
    dates <- extract_and_format_date(basename(files)) # nolint
    sorted_indices <- order(dates)  # Indices des dates triées
    files <- files[sorted_indices]  # Trier les fichiers selon les dates
    dates <- dates[sorted_indices]  # Trier les dates
    choices <- setNames(basename(files), dates)
    selectInput("file", "Choisir une nuit:", choices = choices)
  })
  
  #Chargement du fichier .asc sélectionné
  data <- reactive({
    req(input$file)
    selected_stage <- input$stage
    selected_athlete <- input$athlete
    selected_file <- input$file
    file_path <- file.path(root_dir, selected_athlete, selected_stage, selected_file)
    read_asc(file_path)
  })
  
  #
  output$dataPlot <- renderPlotly({
    req(data())
    
    # Définition des seuils
    spo2_max <- 95
    spo2_min <- 85
    seuil <- input$seuil # Utilisation de la valeur du curseur
    
    # Traitement des données brutes et calcul de la charge hypoxique
    donnees <- process_row_data(data(), spo2_max, spo2_min, seuil)
    calcul_hypoxie <- calculate_burden(donnees, seuil)
    
    # Plot des données
    plot_data(donnees, spo2_max, spo2_min, seuil, calcul_hypoxie$HB, calcul_hypoxie$REDTA)
  })
  
## Partie "Evolution Charge Hypoxique"

# Calcul des données pour l'athlète sélectionné
selected_data <- reactive({
  req(input$athlete_global, input$seuil_global)
  
  # Appel à la fonction process_athlete_data avec root_dir
  process_athlete_data(
    athlete_name = input$athlete_global,
    root_dir = root_dir,
    seuil = input$seuil_global,
    spo2_max = 95,
    spo2_min = 85
  )
})

# Graphique HB
output$hbPlot <- renderPlotly({
  data <- selected_data()  # Récupérer les données calculées
  # Appeler la fonction create_hb_plot avec les données
  create_hb_plot(data)
})

# Graphique REDTA
output$redtaPlot <- renderPlotly({
  data <- selected_data()  # Récupérer les données calculées
  # Appeler la fonction create_redta_plot avec les données
  create_redta_plot(data)
})

output$downloadData <- downloadHandler(
  filename = function() {
    paste("donnees_hypoxie_",input$seuil_global,"_", input$athlete_global, "_", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    # Récupérer les données calculées
    data <- selected_data()
    req(data) # Vérifier que les données existent
    
    # Écrire les données dans un fichier CSV
    write.csv(data, file, row.names = FALSE, fileEncoding = "UTF-8")
  }
)

observeEvent(input$submit_changes, {
  # Initialiser le message de statut
  status_message <- ""

  # Créer un nouvel athlète
  if (input$new_athlete != "") {
    athlete_path <- file.path(root_dir, input$new_athlete)
    if (!dir.exists(athlete_path)) {
      dir.create(athlete_path)
      status_message <- paste(status_message, "Athlète créé:", input$new_athlete, "\n")
    } else {
      status_message <- paste(status_message, "Le dossier de l'athlète existe déjà.\n")
    }
  }

  # Créer un nouveau stage
  if (input$new_stage != "" && input$existing_athlete != "") {
    stage_path <- file.path(root_dir, input$existing_athlete, input$new_stage)
    if (!dir.exists(stage_path)) {
      dir.create(stage_path)
      status_message <- paste(status_message, "Stage créé:", input$new_stage, "pour l'athlète", input$existing_athlete, "\n")
    } else {
      status_message <- paste(status_message, "Le dossier du stage existe déjà.\n")
    }
  }

  # Ajouter des fichiers bruts
  if (!is.null(input$new_files)) {
    files <- input$new_files$datapath
    filenames <- input$new_files$name
    target_dir <- if (input$new_stage != "" && input$existing_athlete != "") {
      file.path(root_dir, input$existing_athlete, input$new_stage)
    } else if (input$existing_athlete != "") {
      file.path(root_dir, input$existing_athlete)
    } else {
      NULL
    }

    if (!is.null(target_dir) && dir.exists(target_dir)) {
      for (i in seq_along(files)) {
        file.copy(files[i], file.path(target_dir, filenames[i]))
      }
      status_message <- paste(status_message, "Fichiers ajoutés dans:", target_dir, "\n")
    } else {
      status_message <- paste(status_message, "Erreur: le dossier cible n'existe pas.\n")
    }
  }

  # Afficher le statut des actions
  output$action_status <- renderText({ status_message })
})
}


# LANCEMENT DE L'APPLI
shinyApp(ui = ui, server = server)


