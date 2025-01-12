# Packages nécessaires
packages <- c("shiny", "leaflet", "ggplot2", "dplyr", "lubridate", "plotly", 
              "DT", "flexdashboard", "shinythemes", "scales", "maps")

options(repos = c(CRAN = "https://cran.rstudio.com"))
options(repos = c(CRAN = "https://packagemanager.rstudio.com/cran/__linux__/focal/latest"))



# Installation et chargement des packages
invisible(lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}))

# Fonctions utilitaires
load_and_clean_data <- function(file_path) {
  data <- read.csv(file_path)
  
  # Conversion des dates
  data$time <- as.POSIXct(data$time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  
  # Vérification et nettoyage des colonnes si elles existent
  if ("dmin" %in% colnames(data)) {
    data$dmin <- ifelse(is.na(data$dmin), median(data$dmin, na.rm = TRUE), data$dmin)
  }
  
  if ("magError" %in% colnames(data)) {
    data$magError <- ifelse(is.na(data$magError), median(data$magError, na.rm = TRUE), data$magError)
  }
  
  # Filtrage des données nécessaires et suppression des valeurs manquantes essentielles
  data <- data %>%
    filter(!is.na(mag), !is.na(depth)) %>%  # Exclure les valeurs manquantes critiques
    select(time, mag, depth, latitude, longitude, place)  # Garder les colonnes utiles
  
  return(data)
}


# Configuration de la palette de couleurs
create_color_palette <- function(data) {
  colorNumeric(
    palette = c("#FF7F7F", "#FF6666", "#FF4500", "#D03028", "#AD2823"),
    domain = data$mag,
    na.color = "transparent"
  )
}
# UI
ui <- fluidPage(
  theme = shinytheme("journal"),
  tags$head(
    tags$style(HTML("
      .dataTables_wrapper {
        background-color: white !important;
        color: black !important;
      }
      .dataTables_length label, 
      .dataTables_filter label,
      .dataTables_info,
      .paginate_button {
        color: black !important;
      }
      .table.dataTable tbody tr.selected {
        background-color: #f5f5f5 !important;
        color: black !important;
      }
      .table.dataTable tbody tr:hover {
        background-color: #f8f9fa !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        background: white !important;
        color: black !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background: #e9ecef !important;
        color: black !important;
      }
    "))
  ),
  
  titlePanel("Analyse interactive des séismes aux États-Unis"),
  
  navbarPage(
    title = NULL,
    id = "navbar",
    
    # Page d'accueil (conservée comme dans l'original)
    tabPanel(
      "Accueil",
      fluidRow(
        column(
          width = 12,
          h2("Contexte de l'étude"),
          p("Cette application permet d'explorer les séismes enregistrés aux États-Unis et au Mexique. 
            L'objectif est de fournir une visualisation interactive des caractéristiques temporelles et géographiques des séismes. 
            Les assureurs peuvent s'y référer afin de pouvoir ajuster la prime commerciale en fonction du lieu de résidence dans le cadre d'une assurance MRH par exemple."),
          h2("Les données source"),
          p(
            "Les données proviennent du site USGS (United States Geological Survey) et contiennent des informations 
            sur la magnitude, la profondeur, les coordonnées géographiques, ainsi que la date et l'heure des séismes, ainsi que d'autres variables apportant quelques détails.
            Voici le lien source des données : ",
            tags$a(href = "https://earthquake.usgs.gov/earthquakes/search/", 
                   target = "_blank", "Cliquez ici pour accéder aux données.")
          ),
          h3("Résumé des données"),
          tableOutput("data_summary"),
          h3("Carte des séismes"),
          plotOutput("static_map", height = "500px")
        )
      )
    ),
    
    # Carte interactive
    tabPanel(
      "Carte interactive",
      sidebarLayout(
        sidebarPanel(
          sliderInput("radius", "Taille des points", min = 1, max = 10, value = 3),
          sliderInput("magnitude", "Filtrer par magnitude :", min = 1, max = 6, value = c(1, 6)),
          sliderInput("depth", "Filtrer par profondeur :", min = -10, max = 150, value = c(0, 100)),
          actionButton("reset", "Réinitialiser les filtres"),
          downloadButton("downloadData", "Télécharger les données filtrées")
        ),
        mainPanel(
          leafletOutput("map", height = 400),
          DTOutput("table")
        )
      )
    ),
    # Nouvel onglet regroupé
    tabPanel(
      "Graphiques intéressants",
      fluidRow(
        column(
          width = 12,
          h3("Aperçu des graphiques"),
          p("Dans cette section, vous trouverez une synthèse visuelle des séismes enregistrés. Les graphiques permettent d'explorer les tendances temporelles, les distributions des magnitudes, les comparaisons régionales et les corrélations entre les caractéristiques des séismes."),
          p("Ces visualisations offrent des insights clés pour mieux comprendre la dynamique et la répartition des séismes.")
        )
      ),
      fluidRow(
        column(
          width = 12,
          h3("Graphique temporel"),
          plotlyOutput("time_series"),
          h3("Distribution des magnitudes"),
          plotlyOutput("magnitude_distribution"),
          h3("Comparaison régionale"),
          plotlyOutput("regional_comparison"),
          h3("Corrélation Magnitude vs Profondeur"),
          plotlyOutput("magnitude_depth_correlation")
        ))),
      tabPanel(
        "Synthèse",
        fluidRow(
          column(
            width = 12,
            h2("Synthèse sur les séismes et l'assurance"),
            p("Les séismes sont des phénomènes naturels imprévisibles qui peuvent causer des dégâts significatifs aux infrastructures et engendrer des pertes humaines et financières."),
            p("Dans un cadre assurantiel, il est crucial de comprendre ces phénomènes pour évaluer les risques associés et ajuster les primes d'assurance. Les visualisations proposées dans cette application permettent aux assureurs de :"),
            tags$ul(
              tags$li("Analyser les tendances historiques des séismes."),
              tags$li("Identifier les zones géographiques les plus exposées."),
              tags$li("Évaluer la corrélation entre la magnitude et la profondeur des séismes.")
            ),
            p("L'intégration de ces données dans des modèles actuariels permet de mieux quantifier les risques et d'établir des stratégies adaptées pour couvrir les sinistres majeurs."),
            h3("Qu'est-ce qu'un séisme ?"),
            p("Un séisme est une vibration brusque de la surface de la Terre causée par la libération d'énergie accumulée sous forme de tension dans les roches. Cette énergie est souvent due au déplacement des plaques tectoniques le long des failles géologiques. Les ondes sismiques produites se propagent à travers le sol, provoquant des secousses pouvant causer des dommages aux infrastructures, des pertes humaines et des changements géologiques."),
            h3("Qu'est-ce que la magnitude ?"),
            p("La magnitude d'un séisme est une mesure de l'énergie libérée lors de celui-ci. Elle est déterminée sur une échelle logarithmique, comme l'échelle de Richter ou l'échelle de moment sismique (Mw). Chaque augmentation d'un point sur l'échelle représente une énergie environ 32 fois plus importante."),
            div(
              style = "text-align: center;",
              img(src = "Richter.jpeg", height = "300px", width = "600px", alt = "Image sur les séismes")
            )
          )
        )
      )
    )
)

# Server
server <- function(input, output, session) {
  
  # Chargement des données réactif
  data <- reactive({
    load_and_clean_data("data/Data1.csv")
  })
  
  # Données filtrées réactives
  filtered_data <- reactive({
    data() %>%
      filter(
        mag >= input$magnitude[1],
        mag <= input$magnitude[2],
        depth >= input$depth[1],
        depth <= input$depth[2]
      )
  })
  
  # Résumé des données
  output$data_summary <- renderTable({
    data() %>%
      summarise(
        `Nombre d'observations` = n(),
        `Dates d'observations` = paste("Du ",min(as.Date(time), na.rm = TRUE), "au", max(as.Date(time), na.rm = TRUE)),
        `Magnitudes observées` = paste(
          "Min :", round(min(mag, na.rm = TRUE), 2), 
          "| Max :", round(max(mag, na.rm = TRUE), 2),
          "| Moyenne :", round(mean(mag, na.rm = TRUE), 2)
        ),
        `Profondeurs observées` = paste(
          "Min :", round(min(depth, na.rm = TRUE), 2), 
          "| Max :", round(max(depth, na.rm = TRUE), 2),
          "| Moyenne :", round(mean(depth, na.rm = TRUE), 2)
        )
      ) %>%
      t() %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "Description") %>%
      rename(Valeurs = V1)
  })
  
  # Carte statique
  output$static_map <- renderPlot({
    usa <- map_data("state")
    ggplot() +
      geom_polygon(data = usa, aes(x = long, y = lat, group = group), 
                   fill = "gray90", color = "white") +
      geom_point(data = data(), aes(x = longitude, y = latitude, 
                                    color = mag, size = mag), 
                 alpha = 0.7) +
      scale_color_gradient(low = "blue", high = "red", name = "Magnitude") +
      scale_size_continuous(range = c(1, 5), name = "Magnitude") +
      theme_minimal() +
      labs(title = "Carte des séismes", x = "Longitude", y = "Latitude") +
      coord_fixed(1.3)
  })
  
  # Carte interactive
  output$map <- renderLeaflet({
    pal <- create_color_palette(data())
    
    leaflet(filtered_data()) %>%
      addTiles() %>%
      setView(lng = -98, lat = 38, zoom = 4) %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        radius = ~mag * input$radius / 2,
        color = ~pal(mag),
        stroke = FALSE,
        fillOpacity = 0.8,
        popup = ~paste(
          "<b>Lieu :</b>", place, "<br>",
          "<b>Date :</b>", format(time, "%d %B %Y"), "<br>",
          "<b>Magnitude :</b>", mag, "<br>",
          "<b>Profondeur :</b>", depth, "km"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~mag,
        title = "Magnitude",
        opacity = 0.8
      )
  } )
  
  output$table <- renderDT({
    datatable(
      filtered_data() %>%
        select(time, latitude, longitude, mag, depth),
      options = list(
        pageLength = 10,
        language = list(
          search = "Rechercher:",
          lengthMenu = "Afficher _MENU_ entrées",
          info = "Affichage de _START_ à _END_ sur _TOTAL_ entrées",
          paginate = list(
            previous = "Précédent",
            `next` = "Suivant"
          )
        )
      ),
      selection = "single",
      class = 'hover stripe',
      style = 'bootstrap',
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'lfrtip',
        initComplete = JS(
          "function(settings, json) {
          $(this.api().table().container()).css({
            'background-color': '#fff',
            'color': '#333'
          });
          $('.dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate') .css({
            'color': '#333'
          });
        }"
        )
      )
    ) %>%
      formatStyle(
        columns = 1:5,  # Toutes les colonnes
        backgroundColor = 'white',
        color = 'black'
      ) %>%
      formatStyle(
        columns = 1:5,
        target = 'row',
        backgroundColor = styleEqual(1, '#f5f5f5')  # Style pour les lignes sélectionnées
      )
  })
  
  # Dans la partie serveur, ajoutez un observeEvent pour gérer la sélection du tableau
  # et mettre à jour la carte
  
  observeEvent(input$table_rows_selected, {
    if (!is.null(input$table_rows_selected)) {
      selected_point <- filtered_data()[input$table_rows_selected, ]
      
      leafletProxy("map") %>%
        clearMarkers() %>%  # Efface les marqueurs existants
        # Ajoute tous les points en gris clair
        addCircleMarkers(
          data = filtered_data(),
          ~longitude, ~latitude,
          radius = ~mag * input$radius / 2,
          color = "#CCCCCC",
          stroke = FALSE,
          fillOpacity = 0.4,
          popup = ~paste(
            "<b>Lieu :</b>", place, "<br>",
            "<b>Date :</b>", format(time, "%d %B %Y"), "<br>",
            "<b>Magnitude :</b>", mag, "<br>",
            "<b>Profondeur :</b>", depth, "km"
          )
        ) %>%
        # Ajoute le point sélectionné en surbrillance
        addCircleMarkers(
          data = selected_point,
          ~longitude, ~latitude,
          radius = ~mag * input$radius,  # Plus grand pour le point sélectionné
          color = "red",
          stroke = TRUE,
          weight = 2,
          fillOpacity = 1,
          popup = ~paste(
            "<b>Lieu :</b>", place, "<br>",
            "<b>Date :</b>", format(time, "%d %B %Y"), "<br>",
            "<b>Magnitude :</b>", mag, "<br>",
            "<b>Profondeur :</b>", depth, "km"
          )
        ) %>%
        setView(
          lng = selected_point$longitude, 
          lat = selected_point$latitude, 
          zoom = 5  # Zoom sur le point sélectionné
        )
    } else {
      # Si aucune ligne n'est sélectionnée, réaffiche tous les points normalement
      leafletProxy("map") %>%
        clearMarkers() %>%
        addCircleMarkers(
          data = filtered_data(),
          ~longitude, ~latitude,
          radius = ~mag * input$radius / 2,
          color = ~pal(mag),
          stroke = FALSE,
          fillOpacity = 0.8,
          popup = ~paste(
            "<b>Lieu :</b>", place, "<br>",
            "<b>Date :</b>", format(time, "%d %B %Y"), "<br>",
            "<b>Magnitude :</b>", mag, "<br>",
            "<b>Profondeur :</b>", depth, "km"
          )
        )
    }
  })

  
  # Graphiques d'analyse (tous conservés comme dans l'original)
  output$time_series <- renderPlotly({
    data() %>%
      mutate(date = as.Date(time)) %>%
      group_by(date) %>%
      summarise(
        Total_Séismes = n(),
        Magnitude_Moyenne = mean(mag, na.rm = TRUE)
      ) %>%
      plot_ly(x = ~date, y = ~Total_Séismes, 
              type = 'scatter', mode = 'lines', 
              name = "Nombre de séismes", 
              line = list(color = "blue")) %>%
      add_trace(y = ~Magnitude_Moyenne, 
                name = "Magnitude moyenne", 
                yaxis = "y2", 
                line = list(color = "red")) %>%
      layout(
        title = "Évolution des séismes dans le temps",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Nombre de séismes"),
        yaxis2 = list(overlaying = "y", 
                      side = "right", 
                      title = "Magnitude moyenne"),
        legend = list(orientation = "h")
      )
  })
  
  
  # Les autres graphiques sont conservés à l'identique...
  output$magnitude_distribution <- renderPlotly({
    plot_ly(data(), x = ~mag, type = "histogram", nbinsx = 20) %>%
      layout(
        title = "Répartition des Magnitudes",
        xaxis = list(title = "Magnitude"),
        yaxis = list(title = "Nombre de Séismes")
      )
  })
  
  output$regional_comparison <- renderPlotly({
    data() %>%
      mutate(
        region = case_when(
          longitude < -115 ~ "Ouest",
          longitude >= -115 & longitude < -100 ~ "Centre",
          TRUE ~ "Est"
        ),
        date = as.Date(time)
      ) %>%
      group_by(region, date) %>%
      summarise(n = n(), .groups = "drop") %>%
      plot_ly(
        x = ~date,
        y = ~n,
        color = ~region,
        type = "bar"
      ) %>%
      layout(
        title = "Comparaison des Séismes par Région",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Nombre de Séismes"),
        barmode = "stack"
      )
  })
  
  
  output$magnitude_depth_correlation <- renderPlotly({
    plot_ly(
      data = data(),
      x = ~depth,
      y = ~mag,
      type = "scatter",
      mode = "markers",
      color = ~mag,
      colors = "Blues"
    ) %>%
      layout(
        title = "Corrélation entre Magnitude et Profondeur",
        xaxis = list(title = "Profondeur (km)"),
        yaxis = list(title = "Magnitude")
      )
  })
  
  # Gestion du tableau
  output$table <- renderDT({
    datatable(
      filtered_data() %>%
        select(time, latitude, longitude, mag, depth),
      options = list(pageLength = 10),
      selection = "single"
    )
  })
  
  # Gestion des événements
  observeEvent(input$reset, {
    updateSliderInput(session, "magnitude", value = c(1, 6))
    updateSliderInput(session, "depth", value = c(0, 100))
    selectRows(dataTableProxy("table"), NULL)
  })

  # Téléchargement des données
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("seismes_filtres_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Lancement de l'application
shinyApp(ui = ui, server = server)