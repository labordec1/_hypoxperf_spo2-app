#### HYPOXPERF- SPo2 - Analyse de la charge hypoxique des athlètes ####

#### PARAMETRAGE ####

##### CHARGEMENT DES LIBRAIRIES ====
library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(DescTools)
library(zoo)
library(bslib)
library(plotly)

##### DEFINITION DE L'ESPACE DE TRAVAIL =====
app_dir <- getwd()
root_dir <- file.path(app_dir, "data", "PROD")

##### CHARGEMENT DES FONCTIONS ====
source("R/data_processing.R")
source("R/plotting.R")

##### PARAMÈTRES ====
spo2_max <- 95
spo2_min <- 85

##### LISTING DES ATHLETES ====
athletes <- list.dirs(root_dir, recursive = FALSE)

#### FRONTEND ####

ui <- page_navbar(
  title = "HYPOXPERF - SpO2",
  bg = "#2D89C8",
  inverse = TRUE,
  
  ##### ONGLET : "Visualisation SpO2" ====
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
  
  ##### ONGLET : "Évolution Charge Hypoxique" ====
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


#### BACKEND ####

server <- function(input, output, session) {
  
##### ONGLET : "Visualisation SpO2" ====
  
  # Sélection du stage
  output$stageSelector <- renderUI({
    req(input$athlete)
    selected_athlete <- input$athlete
    stage_dirs <- list.dirs(file.path(root_dir, selected_athlete), recursive = FALSE)
    selectInput("stage", "Choisir un stage:", choices = basename(stage_dirs))
  })
  
  # Sélection de la nuit
  output$fileSelector <- renderUI({
    #Vérifier la présence d'athlète et de stage
    req(input$athlete, input$stage)
    #Extraction de la saisie utilisateur (stage + athlète)
    selected_stage <- input$stage
    selected_athlete <- input$athlete
    #Listing des fichiers présents dans le dossier choisi par l'utilisateur
    files <- list.files(file.path(root_dir, selected_athlete, selected_stage), pattern = "\\.asc$", full.names = TRUE)
    #Extraction et formatage de la date à partir des noms de fichiers
    dates <- extract_and_format_date(basename(files))
    #Tri par ordre chronologique
    sorted_indices <- order(dates)
    files <- files[sorted_indices] 
    dates <- dates[sorted_indices]
    #Création de la liste des choix de nuit pour l'utilisateur
    choices <- setNames(basename(files), dates)
    #Menu déroulant pour choisir la nuit
    selectInput("file", "Choisir une nuit:", choices = choices)
  })
  
  #Chargement du fichier .asc sélectionné (réactualisation à chaque changement de sélection)
  data <- reactive({
    #Verifier la présence du fichier
    req(input$file)
    #Sélection de l'utilisateur (athlète, stage, file)
    selected_stage <- input$stage
    selected_athlete <- input$athlete
    selected_file <- input$file
    #Création du chemin du fichier
    file_path <- file.path(root_dir, selected_athlete, selected_stage, selected_file)
    #Lecture du fichier via fonction "read_asc"
    read_asc(file_path)
  })
  
  # Affichage du graphique interactif de la SpO2 via plotly 
  output$dataPlot <- renderPlotly({
    req(data())
    
    # Définition du seuil de désaturation par l'utilisateur
    seuil <- input$seuil
    
    # Traitement des données brutes et calcul de la charge hypoxique
    donnees <- process_row_data(data(), spo2_max, spo2_min, seuil)
    calcul_hypoxie <- calculate_burden(donnees, seuil)
    
    # Plot des données
    plot_spo2_by_night(donnees, spo2_max, spo2_min, seuil, calcul_hypoxie$HB, calcul_hypoxie$REDTA)
  })
  
##### ONGLET : "Évolution Charge Hypoxique" ====

# Calcul de la charge hypoxique pour l'athlète sélectionné
selected_data <- reactive({
  req(input$athlete_global, input$seuil_global)
  
  # Appel à la fonction process_athlete_data
  process_athlete_data(
    athlete_name = input$athlete_global,
    root_dir = root_dir,
    seuil = input$seuil_global,
    spo2_max, 
    spo2_min
  )
})

# Plot graphique HB
output$hbPlot <- renderPlotly({
  data <- selected_data()
  create_hb_plot(data)
})

# Plot graphique REDTA
output$redtaPlot <- renderPlotly({
  data <- selected_data()
  create_redta_plot(data)
})

# Extraction des données en fichier .csv
output$downloadData <- downloadHandler(
  #Création du nom de fichier
  filename = function() {
    paste("donnees_hypoxie_",input$seuil_global,"_", input$athlete_global, "_", Sys.Date(), ".csv", sep = "")
  },
  #Création du contenu du fichier
  content = function(file) {
    data <- selected_data()
    req(data)
    write.csv(data, file, row.names = FALSE, fileEncoding = "UTF-8")
  }
)
}


#### LANCEMENT APPLI ####
shinyApp(ui = ui, server = server)
