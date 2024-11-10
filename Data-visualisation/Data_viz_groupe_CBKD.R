#Projet Data-visualisation sur les séismes à forte magnétude à l'ouest des Etats-Unis

library(ggplot2)
library(shiny)
library(leaflet)

#Etape 1 : découverte des données 

# Charger les données
data <- read.csv("data1.csv")

# Afficher un aperçu des données
head(data)

# Résumé statistique des données
summary(data)

# Structure des données
str(data)

#Etape 2 : Passage sur R Shiny

# Interface utilisateur
library(shiny)
library(leaflet)

# UI : Interface utilisateur avec deux onglets
ui <- fluidPage(
  titlePanel("Application sur les Séismes aux États-Unis"),
  
  # Utilisation de tabsetPanel pour les fenêtres multiples
  tabsetPanel(
    tabPanel("Carte",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("radius", "Taille des points", min = 1, max = 10, value = 3),
                 helpText("Ajustez la taille des points pour une meilleure visibilité.")
               ),
               mainPanel(
                 leafletOutput("map", height = 600)
               )
             )
    ),
    tabPanel("Résumé des données",
             mainPanel(
               tableOutput("summary")
             )
    )
  )
)

# Serveur : logique de l'application
server <- function(input, output) {
  
  # Carte des séismes
  output$map <- renderLeaflet({
    leaflet(data = data) %>%
      addTiles() %>%
      addCircleMarkers(~longitude, ~latitude,
                       radius = ~input$radius,
                       color = "red",
                       stroke = FALSE,
                       fillOpacity = 0.6,
                       popup = ~paste("Magnitude:", mag))
  })
  
  # Résumé des données
  output$summary <- renderTable({
    summary(data)  # Résumé statistique des données
  })
}

# Lancement de l'application
shinyApp(ui = ui, server = server)
