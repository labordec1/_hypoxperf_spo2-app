#### LISTE DES FONCTIONS POUR VISUALISATION ####

##### CHARGEMENT DES LIBRAIRIES ====
library(ggplot2)
library(dplyr)
library(plotly)


##### Plot SpO2 par nuit ====
#' Permet d'afficher la courbe d'évolution de la SpO2 d'un athlète pour une nuit
#' 
#' @param donnees tibble des donnees issues des fichiers .asc
#' @param spo2_max valeur haute limite des données acceptables
#' @param spo2_min valeur basse limite des données acceptables
#' @param seuil valeur du seuil de désaturation
#' @param HB valeur charge hypoxique calculée (HB)
#' @param REDTA valeur charge hypoxique calculée (REDTA)
#' @return un graphique interactif (ggplotly)

plot_spo2_by_night <- function(donnees, spo2_max, spo2_min, seuil, HB, REDTA) {
  # Fonction permettant de calculer les paramètres de lissage
  param_smooth <- smooth.spline(donnees$heure, donnees$spo2)
  # Conversion des heures en format numérique
  donnees$heure_numeric <- as.numeric(donnees$heure)
  # A partir des paramètres de lissage, calcul des nouvelles valeurs de SpO2
  donnees$smoothed_spline <- predict(param_smooth, donnees$heure_numeric)$y
  
  # Création du plot
  p <- ggplot(donnees, aes(x = duree_cumulee, y = spo2)) +
    # Courbe lissée Sp02
    geom_line(aes(y = smoothed_spline), color = "red", size = 0.5) +
    # Seuil max
    geom_hline(yintercept = spo2_max, linetype = "dashed", color = "red", size = 1) +
    # Seuil min
    geom_hline(yintercept = spo2_min, linetype = "dashed", color = "blue", size = 1) +
    # Seuil de désaturation
    geom_hline(yintercept = seuil, linetype = "dashed", color = "green", size = 1) +
    # Coloration de l'aire sous seuil
    geom_ribbon(data = donnees %>% filter(sous_seuil), 
                aes(ymin = smoothed_spline, 
                    ymax = seuil, 
                    group = grp), 
                fill = "blue", 
                alpha = 0.3) +
    labs(title = "Évolution de la SpO2 au cours du temps",
         x = "Temps",
         y = "SpO2") +
    # Label HB et REDTA, position en bas à droite
    annotate("text", x = max(donnees$duree_cumulee)-1000, y = max(donnees$spo2) - 8,
             label = paste("HB =", round(HB, 1), "%", "\nREDTA =", round(REDTA, 1), "%h"),
             hjust = 1, vjust = 1, color = "black", size = 4)
  
  ggplotly(p)
}

##### Plot évolution HB par stage ====
#' Permet d'afficher la courbe d'évolution de la charge hypoxique par athlète, par stage
#' 
#' @param data tibble de données contenant les valeurs de HB et REDTA issue des calculs charge hypoxique
#' @return un graphique interactif (ggplotly)

create_hb_plot <- function(data) {
  p <- ggplot(data, aes(x = Nuit)) +
    # Courbe d'évolution HB
    geom_line(aes(y = HB, group = Stage, color = Stage)) +
    # Label 
    labs(title = "Évolution de HB par stage",
         x = "Nombre de nuit",
         y = "HB (%)",
         color = "Stage") +
    theme_minimal()
  
  ggplotly(p)
}

##### Plot évolution REDTA par stage ====
#' Permet d'afficher la courbe d'évolution de la charge hypoxique (REDTA) par athlète, par stage
#' 
#' @param data tibble de données contenant les valeurs de HB et REDTA issue des calculs charge hypoxique
#' @return un graphique interactif (ggplotly)
#' 

create_redta_plot <- function(data) {
  p <- ggplot(data, aes(x = Nuit)) +
    # Courbe d'évolution REDTA
    geom_line(aes(y = REDTA, group = Stage, color = Stage)) +
    # Label 
    labs(title = "Évolution de REDTA par stage",
         x = "Nombre de nuit",
         y = "REDTA (%h)",
         color = "Stage") +
    theme_minimal()
  
  ggplotly(p)
}

##### Création de l'affichage groupé ====
#' Permet d'afficher les deux graphiques HB et REDTA
#' 
#' @param data tibble de données contenant les valeurs de HB et REDTA issue des calculs charge hypoxique
#' @return affichage des deux graphiques dans l'interface
#' 
generate_athlete_plots <- function(data) {
  fig1 <- create_hb_plot(data)
  fig2 <- create_redta_plot(data)
  
  subplot(fig1, fig2, titleX = TRUE)
}
