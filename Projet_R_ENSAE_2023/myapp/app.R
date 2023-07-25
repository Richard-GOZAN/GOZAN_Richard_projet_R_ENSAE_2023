library(sp)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)

# Chargement des données géographiques de l'Afrique de l'Ouest
afrique_ouest <- subset(ne_countries(scale = "medium", continent = "Africa"), subregion == "Western Africa")
donnees <- read.csv("ACLED-Western_Africa.csv")

# Création de l'interface utilisateur
ui <- fluidPage(
  # Titre de l'application
  titlePanel("Carte interactive de l'Afrique de l'Ouest"),
  
  # Sidebar avec les options de filtrage
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        inputId = "pays",
        label = "Sélectionnez un ou plusieurs pays",
        choices = unique(donnees$pays),
        selected = unique(donnees$pays),
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      pickerInput(
        inputId = "type_evenement",
        label = "Sélectionnez un ou plusieurs types d'événements",
        choices = unique(donnees$type),
        selected = unique(donnees$type),
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      pickerInput(
        inputId = "annee",
        label = "Sélectionnez une ou plusieurs années",
        choices = unique(donnees$annee),
        selected = unique(donnees$annee),
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      )
    ),
    
    # Affichage de la carte Leaflet
    mainPanel(
      leafletOutput(outputId = "carte",
                    width = "100%",
                    height = "720px")
    )
  )
)

# Logique du serveur
server <- function(input, output, session) {
  filtered_data <- reactive({
    donnees %>%
      filter(pays %in% input$pays &
               type %in% input$type_evenement & 
               annee %in% input$annee)
  })
  
  output$carte <- renderLeaflet({
    donnees_filtrage <- filtered_data()
    
    # Création de la carte Leaflet
    carte_leaflet <- leaflet() %>%
      setView(lng = 0, lat = 8, zoom = 3) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(data = ne_countries(type = "countries", country = input$pays),
                  fillColor = "lightblue", color = "gray", fillOpacity = 0.6) %>%
      addCircleMarkers(data = donnees_filtrage,
                       lng = ~longitude, lat = ~latitude,
                       radius = 3, fillOpacity = 0.7)
    
    carte_leaflet
  })
}

# Lancement de l'application Shiny
shinyApp(ui = ui, server = server)