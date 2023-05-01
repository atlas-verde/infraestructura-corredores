#
# Dashboard for "Infraestructura verde, gris y azul - Corredores biológicos"
#


# PACKAGES
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(sf)
library(terra)
library(raster)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(colorspace)


# FUNCTIONS

# Create map
create_map <-
  function() {
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Mapa de calles (OpenStreetMap)") |>
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Mapa oscuro (CartoDB Dark Matter)") |>
      addProviderTiles(providers$Stamen.TonerLite, group = "Mapa claro (Stamen Toner Lite)") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Imágenes satelitales (ESRI World Imagery)") |>
      addWMSTiles(
        WMS_CATIE_URL,
        layers = WMS_INFRASTRUCTURE_LAYER,
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          crs = "EPSG:4326"
        ),
        group = "Infraestructura verde, gris y azul"
      ) |>
      addWMSLegend(
        uri = paste0(
          WMS_CATIE_URL, "?",
          "REQUEST=GetLegendGraphic&VERSION=1.0.0", 
          "&FORMAT=image/png&WIDTH=20&HEIGHT=15&LAYER=infraestructura"
        )
      ) |>
      addPolygons(
        data = corridors,
        layerId = ~nombre_cb,
        fillOpacity = 0.0,
        stroke = TRUE,
        color = "#E10600",
        weight = 3,
        popup = paste(
          paste("<strong>Corredor biológico:</strong>",  corridors[[COLUMN_CORRIDOR_NAME]])
        ),
        label = paste(
          paste("Corredor biológico:",  corridors[[COLUMN_CORRIDOR_NAME]])
        ),
        group = "Corredores biológicos"
      ) |>      
      addLayersControl(
        baseGroups = c(
          "Mapa de calles (OpenStreetMap)",
          "Mapa oscuro (CartoDB Dark Matter)",
          "Mapa claro (Stamen Toner Lite)",
          "Imágenes satelitales (ESRI World Imagery)"
        ),
        overlayGroups = c("Infraestructura verde, gris y azul", "Corredores biológicos"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE)
      ) |>      
      addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) |>
      addMouseCoordinates() |>
      addSearchOSM() |>
      addResetMapButton() |>
      addFullscreenControl()
  }

# Create barplot
create_barplot <-
  function(corridor) {
    # Filter
    corridor_data <- infrastructure_corridors |>
      filter(Corredor == corridor) %>%
      gather(key = "category", value = "area",-Corredor)
    
    # Ggplot2 plot
    barplot_infrastructure_ggplot2 <-
      corridor_data |>
      filter(area > 0) |>
      ggplot(aes(x = reorder(category, area), y = area)) +
      geom_col(
        fill = 'dodgerblue4',
        aes(text = paste0(category, ": ", round(area, 1), " ha"))
      ) +
      coord_flip() +
      labs(
        title = paste0("Infraestructura del corredor biológico ", corridor),
        x = element_blank(),
        y = "Área (ha)"
      ) +
      theme_classic(
        base_size = 12,
        # base_family = 'Source Sans Pro'
      )
    
    # Plotly plot
    barplot_infrastructure_ggplot2 |>
      ggplotly(tooltip = "text") |>
      config(locale = 'es')    
  }

  # SAVE THIS FOR LATER USE
  # https://github.com/rstudio/leaflet/issues/496
  setShapeStyle <- function( map, data = getMapData(map), layerId,
                             stroke = NULL, color = NULL,
                             weight = NULL, opacity = NULL,
                             fill = NULL, fillColor = NULL,
                             fillOpacity = NULL, dashArray = NULL,
                             smoothFactor = NULL, noClip = NULL,
                             options = NULL
  ){
    options <- c(list(layerId = layerId),
                 options,
                 filterNULL(list(stroke = stroke, color = color,
                                 weight = weight, opacity = opacity,
                                 fill = fill, fillColor = fillColor,
                                 fillOpacity = fillOpacity, dashArray = dashArray,
                                 smoothFactor = smoothFactor, noClip = noClip
                 )))
    # evaluate all options
    options <- evalFormula(options, data = data)
    # make them the same length (by building a data.frame)
    options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  
    layerId <- options[[1]]
    style <- options[-1] # drop layer column
  
    #print(list(style=style))
    leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style);
  }

# CONSTANTS

# Data sources
WMS_CATIE_URL <- "https://catie.info/geoserver/atlasverde/wms"
WMS_INFRASTRUCTURE_LAYER <- "infraestructura"

DSN_CORRIDORS <- "data/corredores.geojson"
COLUMN_CORRIDOR_NAME <- "nombre_cb"
DSN_INFRASTRUCTURE_CORRIDORS <- "data/infraestructura-corredores.xlsx"

# Corridors
corridors <- st_read(dsn = DSN_CORRIDORS, quiet = TRUE)

# Corridors and infrastructure
infrastructure_corridors <- read_excel(DSN_INFRASTRUCTURE_CORRIDORS, sheet = 1)

# Data cleaning

# USER INTERFACE
ui <- fluidPage(
  
  theme = "bootstrap",
  tags$head(
    tags$style(
      HTML(
        '
        .texto_agradecimiento_logos_1 {
          text-align: center;
        }        
        .texto_agradecimiento_logos_2 {
          text-align: center;
        }'
      )
    )
  ),
  
  navbarPage(
    title = tags$span(
      tags$a(href = "http://atlas-verde.org/", target = "_blank", "Atlas de servicios ecosistémicos de la GAM"),
      " - ",
      "Infraestructura verde, gris y azul - Corredores biológicos"
    ),
    theme = shinytheme("lumen"),

    fluidRow(withSpinner(leafletOutput("map_infrastructure"))),
    fluidRow(h4("Haga clic en el mapa para mostrar los datos de un corredor biológico en el gráfico")),
    fluidRow(withSpinner(plotlyOutput("barplot_infrastructure")))        
  ),
  
  fluidRow(h1(column(width = 12))),
  fluidRow(h1(column(width = 12))),  
  h4(class = "texto_agradecimiento_logos_1", strong("Acerca del Atlas de Servicios Ecosistémicos de la GAM")),
  h4(class = "texto_agradecimiento_logos-2", "El Atlas de Servicios Ecosistémicos de la GAM es producto de la cooperación entre los Gobiernos de Alemania y Costa Rica en el marco del proyecto Biodiver_City – Establecimiento de Corredores Biológicos Interurbanos con el fin de promover el desarrollo urbano centrado en los beneficios de la naturaleza. El instrumento fue desarrollado por el CATIE, por encargo de la Cooperación alemana para el desarrollo GIZ, bajo una estrecha articulación con el MINAE, CENIGA, SINAC y con el apoyo técnico del Instituto de Estudios Ambientales Helmholtz, UFZ."),
  fluidRow(h1(column(width = 12))),
  fluidRow(
    column(width = 4, img(src = "logo-gcr20222026.png", height = 90)),
    column(width = 4, img(src = "logo-minae.png", height = 90)),
    column(width = 4, img(src = "logo-sinac.jpg", height = 90)),
    class = "text-center"
  ),
  fluidRow(h1(column(width = 12))),
  fluidRow(
    column(width = 4, img(src = "logo-catie.jpeg", height = 90)),
    column(width = 4, img(src = "logo-giz.png", height = 90)),
    column(
      width = 4,
      img(src = "logo-minambientealemania-iki.png", height = 90)
    ),
    class = "text-center"
  ),
  fluidRow(h1(column(width = 12))),
  fluidRow(h1(column(width = 12)))  
)


# SERVER LOGIC
server <- function(input, output, session) {
  # Map
  output$map_infrastructure <- renderLeaflet({
    create_map()
  })
  
  # Bar plot
  output$barplot_infrastructure <- renderPlotly({
    create_barplot(selected_corridor())
  })  
  
  # Initial "selected" biological corridor
  selected_corridor <- reactiveVal("Montes del Aguacate")
  
  # Capture click event in corridors layer for zooming and changing styles
  observeEvent(input$map_infrastructure_shape_click, {
    click_data <- input$map_infrastructure_shape_click
    
    if (!is.null(click_data)) {
      print(click_data$id)
      selected_corridor(click_data$id)
      
      # Zoom to selected polygon
      selected_corridor_polygon <- corridors |> filter(nombre_cb == click_data$id)
      leafletProxy("map_infrastructure") |>
        fitBounds(
          lng1 = min(st_bbox(selected_corridor_polygon)[["xmin"]]),
          lat1 = min(st_bbox(selected_corridor_polygon)[["ymin"]]),
          lng2 = max(st_bbox(selected_corridor_polygon)[["xmax"]]),
          lat2 = max(st_bbox(selected_corridor_polygon)[["ymax"]])
        )      
      
      # SAVE THIS FOR LATER
      # Change style of clicked canton
      # Restablecer el estilo del polígono previamente seleccionado
      # if (!is.null(selected_canton())) {
      #   leafletProxy("map_infrastructure") |>
      #     setShapeStyle(layerId = selected_canton(), fillColor="yellow", color = "black")
      #     # setStyle(layerId = selected_polygon(), weight = 1, color = "Black", fillColor = "transparent", fillOpacity = 0)
      # }      
      # # Cambiar el estilo del polígono seleccionado y almacenarlo en la variable reactiva
      # leafletProxy("map_infrastructure") |>
      #   setShapeStyle(layerId = click_data$id, fillColor="blue", color = "blue")
      #   # setStyle(layerId = click_data$id, weight = 3, color = "blue", fillColor = "blue", fillOpacity = 0.3)
      # selected_canton(click_data$id)      
      
    }
  })  
  
}


# RUN APPLICATION
shinyApp(ui = ui, server = server)