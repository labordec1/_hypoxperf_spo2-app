# LISTE DES FONCTIONS DE VISUALISATION

## Chargement des librairies
library(ggplot2)
library(dplyr)
library(plotly)

## Plot de l'évolution de la SpO2 au cours de la nuit

plot_data <- function(donnees, spo2_max, spo2_min, seuil, HB, REDTA) {
  smooth_spline <- smooth.spline(donnees$heure, donnees$spo2)
  donnees$heure_numeric <- as.numeric(donnees$heure)
  donnees$smoothed_spline <- predict(smooth_spline, donnees$heure_numeric)$y
  
  p <- ggplot(donnees, aes(x = duree_cumulee, y = spo2)) +
    geom_line(aes(y = smoothed_spline), color = "red", size = 0.5) +
    geom_hline(yintercept = spo2_max, linetype = "dashed", color = "red", size = 1) +
    geom_hline(yintercept = spo2_min, linetype = "dashed", color = "blue", size = 1) +
    geom_hline(yintercept = seuil, linetype = "dashed", color = "green", size = 1) +
    geom_ribbon(data = donnees %>% filter(sous_seuil), aes(ymin = spo2, ymax = seuil, group = grp), fill = "blue", alpha = 0.3) +
    labs(title = "Évolution de la SpO2 au cours du temps",
         x = "Temps",
         y = "SpO2") +
    annotate("text", x = max(donnees$duree_cumulee)-1000, y = max(donnees$spo2) - 8,
             label = paste("HB =", round(HB, 1), "%", "\nREDTA =", round(REDTA, 1), "%h"),
             hjust = 1, vjust = 1, color = "black", size = 4)
  
  ggplotly(p)
}

## Création du plot du HB de l'athlète sélectionné

create_hb_plot <- function(data) {
  p <- ggplot(data, aes(x = Count)) +
    geom_line(aes(y = HB, group = Stage, color = Stage)) +
    scale_y_continuous(
      name = "HB (%)",
      sec.axis = sec_axis(~ . * 1000, name = "DureeTotale")
    ) +
    labs(title = "Évolution de HB par stage",
         x = "Nombre de nuit",
         color = "Stage") +
    theme_minimal()
  
  ggplotly(p)
}

## Création du plot du REDTA de l'athlète sélectionné
create_redta_plot <- function(data) {
  p <- ggplot(data, aes(x = Count)) +
    geom_line(aes(y = REDTA, group = Stage, color = Stage)) +
    scale_y_continuous(
      name = "REDTA (%h)",
      sec.axis = sec_axis(~ . * 1000, name = "DureeTotale")
    ) +
    labs(title = "Évolution de REDTA par stage",
         x = "Nombre de nuit",
         color = "Stage") +
    theme_minimal()
  
  ggplotly(p)
}

## Plot des graphiques des charges hypoxiques calculées
generate_athlete_plots <- function(data) {
  fig1 <- create_hb_plot(data)
  fig2 <- create_redta_plot(data)
  
  subplot(fig1, fig2, nrows = 1, titleX = TRUE)
}
