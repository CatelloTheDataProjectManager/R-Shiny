library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(rAmCharts)

# UI
ui <- fluidPage(
  titlePanel("Iris data set"),
  sidebarLayout(
    sidebarPanel(
      h3("Graphique de dispersion avec régression"),
      p("Les lignes de régression et leurs équations sont affichées pour chaque espèce."),
      checkboxGroupInput(
        inputId = "species",
        label = "Sélectionner les espèces à afficher :",
        choices = levels(iris$Species),
        selected = levels(iris$Species)
      ),
      sliderInput(
        inputId = "x_range",
        label = "Sélectionner la plage pour la longueur des sépales (Sepal.Length) :",
        min = min(iris$Sepal.Length),
        max = max(iris$Sepal.Length),
        value = c(min(iris$Sepal.Length), max(iris$Sepal.Length))
      ),
      sliderInput(
        inputId = "y_range",
        label = "Sélectionner la plage pour la largeur des sépales (Sepal.Width) :",
        min = min(iris$Sepal.Width),
        max = max(iris$Sepal.Width),
        value = c(min(iris$Sepal.Width), max(iris$Sepal.Width))
      ),
      radioButtons(
        inputId = "plot_type",
        label = "Choisir le type de graphique à afficher :",
        choices = c("Nuages de points", "Boxplot"),
        selected = "Nuages de points"
      )
    ),
    mainPanel(
      uiOutput("plotOutput") # Output pour afficher soit les nuages de points, soit le boxplot
    )
  )
)

# Server
server <- function(input, output) {
  output$plotOutput <- renderUI({
    if (input$plot_type == "Nuages de points") {
      plotOutput("irisScatterPlot") # Output pour les nuages de points
    } else {
      amChartsOutput("amBoxplot") # Output pour le boxplot avec rAmCharts
    }
  })
  
  output$irisScatterPlot <- renderPlot({
    # Filtrer les données en fonction des espèces sélectionnées
    filtered_data <- iris %>%
      filter(Species %in% input$species)
    
    # Recalculer les coefficients de régression pour les espèces sélectionnées
    coefficients <- filtered_data %>%
      group_by(Species) %>%
      summarise(
        intercept = coef(lm(Sepal.Width ~ Sepal.Length, data = cur_data()))[1],
        slope = coef(lm(Sepal.Width ~ Sepal.Length, data = cur_data()))[2]
      )
    
    # Nuages de points pour chaque espèce
    scatter_plot <- ggplot(filtered_data, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
      geom_point() +
      facet_wrap(~ Species) +
      xlim(input$x_range) +
      ylim(input$y_range) +
      labs(title = "Nuages de points pour chaque espèce") +
      theme_minimal()
    
    # Ajout des droites de régression pour chaque espèce
    scatter_plot <- scatter_plot +
      geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
      geom_text(data = coefficients, 
                aes(x = 6, y = 4, 
                    label = paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x")), 
                color = "black", 
                parse = FALSE)
    
    scatter_plot
  })
  
  output$amBoxplot <- renderAmCharts({
    # Création du boxplot pour les 4 variables numériques
    boxplot_data <- iris %>%
      pivot_longer(cols = -Species, names_to = "Variable", values_to = "Value")
    
    amBoxplot(Value ~ Variable, data = boxplot_data, main_title = "Boxplot pour les variables numériques", value_title = "Value", category_title = "Variable")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
