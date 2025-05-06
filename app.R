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

## Chargement des résultats globaux
#results <- read_csv("data/results.csv")



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
  )
)


# DEFINITION DE LA LOGIQUE SERVEUR

server <- function(input, output, session) {
  
## Partie "Visualisation SpO2"
  
  #Sélection du stage
  output$stageSelector <- renderUI({
    req(input$athlete)
    selected_athlete <- input$athlete
    stage_dirs <- list.dirs(file.path(root_dir, selected_athlete), recursive = FALSE)
    selectInput("stage", "Choisir un stage:", choices = basename(stage_dirs))
  })
  
  #Sélection de la nuit
  output$fileSelector <- renderUI({
    req(input$athlete, input$stage)
    selected_stage <- input$stage
    selected_athlete <- input$athlete
    files <- list.files(file.path(root_dir, selected_athlete, selected_stage), pattern = "\\.asc$", full.names = TRUE)
    dates <- extract_and_format_date(basename(files))
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
}


# LANCEMENT DE L'APPLI
shinyApp(ui = ui, server = server)


