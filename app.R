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
results <- read_csv("data/results.csv")



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
        uiOutput("fileSelector")
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
        selectInput("athlete_global", "Choisir un athlète:", choices = basename(athletes))
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
    
    #Définition des seuils
    spo2_max <- 95
    spo2_min <- 85
    seuil <- 92
    
    #Traitement des données bruts, et calcul de la charge hypoxique
    donnees <- process_row_data(data(), spo2_max, spo2_min, seuil)
    calcul_hypoxie <- calculate_burden(donnees, seuil)
    
    #Plot des données
    plot_data(donnees, spo2_max, spo2_min, seuil, calcul_hypoxie$HB, calcul_hypoxie$REDTA)
  })
  
## Partie "Evolution Charge Hypoxique"
  
  selected_data <- reactive({
    filter_results_by_athlete(results, input$athlete_global)
  })
  
  output$hbPlot <- renderPlotly({
    data <- selected_data()
    create_hb_plot(data)
  })

  output$redtaPlot <- renderPlotly({
    data <- selected_data()
    create_redta_plot(data)
  })
}
  

# LANCEMENT DE L'APPLI
shinyApp(ui = ui, server = server)

