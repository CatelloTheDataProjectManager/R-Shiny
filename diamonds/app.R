# Installer les packages nécessaires si ce n'est pas déjà fait
if (!require(shiny)) install.packages("shiny")  # Installer le package shiny s'il n'est pas déjà installé
if (!require(DT)) install.packages("DT")        # Installer le package DT s'il n'est pas déjà installé
if (!require(ggplot2)) install.packages("ggplot2")  # Installer le package ggplot2 s'il n'est pas déjà installé
if (!require(dplyr)) install.packages("dplyr")  # Installer le package dplyr s'il n'est pas déjà installé

library(shiny)     # Charger la bibliothèque shiny
library(DT)        # Charger la bibliothèque DT pour la manipulation des tableaux
library(ggplot2)   # Charger la bibliothèque ggplot2 pour la création de graphiques
library(dplyr)     # Charger la bibliothèque dplyr pour la manipulation des données

# Lire les données depuis un fichier CSV
mydata <- read.csv("C:/Users/Catello/Desktop/IAMDA1/R Shiny - Mme Othman/Partiel/diamonds.csv", sep = ",")

# Interface utilisateur
ui <- navbarPage(
  "Mon Application Shiny",
  tabPanel("Data",    # Onglet pour afficher les données
           fluidPage(
             checkboxInput("show_all", "Afficher toutes les lignes", value = FALSE),  # Case à cocher pour afficher toutes les lignes
             selectInput("cut_filter", "Filtrer par coupe (cut):",  # Sélecteur pour filtrer les données par coupe
                         choices = c("All", unique(mydata$cut))),  
             DTOutput("dataTable"),  # Sortie pour afficher le tableau de données
             verbatimTextOutput("summaryStats")  # Sortie pour afficher le résumé statistique des données
           )
  ),
  tabPanel("Visualisation",   # Onglet pour afficher les visualisations
           fluidPage(
             navlistPanel(    # Panel de navigation pour les différentes visualisations
               "Visualisation",
               tabPanel("Histogramme des prix",    # Onglet pour l'histogramme des prix
                        sliderInput("bins", "Nombre de bins:", min = 1, max = 50, value = 30),  # Contrôle pour le nombre de bins
                        plotOutput("histogramPlot")   # Sortie pour afficher l'histogramme
               ),
               tabPanel("Boîte à moustaches de la profondeur",   # Onglet pour la boîte à moustaches de la profondeur
                        selectInput("boxplotColor", "Couleur de la boîte à moustaches:",  # Sélecteur pour la couleur de la boîte à moustaches
                                    choices = c("blue", "red", "green", "yellow"), selected = "blue"),
                        plotOutput("boxplotPlot")   # Sortie pour afficher la boîte à moustaches
               ),
               tabPanel("Nuage de points: Prix vs Carat",    # Onglet pour le nuage de points Prix vs Carat
                        textInput("scatterTitle", "Titre du nuage de points:", value = "Nuage de points"),  # Champ pour le titre du nuage de points
                        plotOutput("scatterPlot")   # Sortie pour afficher le nuage de points
               )
             )
           )
  )
)

# Serveur
server <- function(input, output) {
  # Réactive pour filtrer les données en fonction de la sélection de l'utilisateur
  filtered_data <- reactive({
    if (input$cut_filter == "All") {
      data <- mydata
    } else {
      data <- filter(mydata, cut == input$cut_filter)
    }
    data
  })
  
  # Affichage du tableau de données
  output$dataTable <- renderDT({
    data <- filtered_data()
    if (input$show_all) {
      datatable(data)
    } else {
      datatable(head(data, 5))
    }
  })
  
  # Affichage du résumé statistique des données
  output$summaryStats <- renderPrint({
    data <- filtered_data()
    summary_stats <- data %>%
      summarise_all(list(
        count = ~n(),
        mean = ~mean(. , na.rm = TRUE),
        sd = ~sd(. , na.rm = TRUE)
      ))
    print(summary_stats)
  })
  
  # Affichage de l'histogramme des prix
  output$histogramPlot <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = price)) +
      geom_histogram(binwidth = input$bins, fill = "blue", color = "black") +
      labs(title = "Histogramme des prix", x = "Prix", y = "Fréquence")
  })
  
  # Affichage de la boîte à moustaches de la profondeur
  output$boxplotPlot <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(y = depth)) +
      geom_boxplot(fill = input$boxplotColor, color = "black") +
      labs(title = "Boîte à moustaches de la profondeur", y = "Profondeur")
  })
  
  # Affichage du nuage de points Prix vs Carat
  output$scatterPlot <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = carat, y = price)) +
      geom_point(color = "red") +
      labs(title = input$scatterTitle, x = "Carat", y = "Prix")
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)
