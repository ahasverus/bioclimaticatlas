library(leaflet)
library(shiny)
library(shinyjs)
library(sp)
library(raster)
library(rgdal)
library(RColorBrewer)

# setwd("~/Desktop/")


species <- readRDS("data/species_list.rds")
climate <- readRDS("data/variables_list.rds")
grd     <- readRDS("data/grid.rds")

server <- function(input, output, session) {

  output$map_climate <- renderLeaflet({

    map_climate <- leaflet(data = grd) %>%
    addProviderTiles(provider = providers$Esri.WorldTopoMap)
  })

  observe({

    ### Update map
    leafletProxy(
      mapId = "map_climate",
      data  = grd
    ) %>%
    fitBounds(
      lng1 = ~-45.50,
      lat1 = ~ 50.00,
      lng2 = ~-75.50,
      lat2 = ~ 63.00
    )
  })

  output$map_species <- renderLeaflet({

    map_species <- leaflet(data = grd) %>%
    addProviderTiles(provider = providers$Esri.WorldTopoMap)
  })

  observe({

    ### Update map
    leafletProxy(
      mapId = "map_species",
      data  = grd
    ) %>%
    fitBounds(
      lng1 = ~-45.50,
      lat1 = ~ 50.00,
      lng2 = ~-75.50,
      lat2 = ~ 63.00
    )
  })

  onclick("nav",

    for (i in c("map_climate", "map_species")) {

      leafletProxy(
        mapId = i,
        data  = grd
      ) %>%

      fitBounds(
        lng1 = ~-45.50,
        lat1 = ~ 50.00,
        lng2 = ~-75.50,
        lat2 = ~ 63.00
      )
    }
  )

  observe({
    if (input$nav == "Climate change")
      showNotification(input$nav)
  })
}
