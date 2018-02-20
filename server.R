library(leaflet)
library(shiny)
library(shinyjs)
library(sp)
library(raster)
library(rgdal)
library(RColorBrewer)

setwd("~/Desktop/")


species <- readRDS("bioclimaticatlas/data/species_list.rds")
grd     <- readRDS("bioclimaticatlas/data/grid.rds")

server <- function(input, output, session) {

  output$map <- renderLeaflet({

    map <- leaflet(
      data = grd
    ) %>%
    addProviderTiles(
      provider = providers$Esri.WorldTopoMap
    )
  })

  observe({


    ### Update map
    leafletProxy(
      mapId = "map",
      data  = grd) %>%
    fitBounds(
      lng1 = ~-45.50,
      lat1 = ~ 50.00,
      lng2 = ~-75.50,
      lat2 = ~ 63.00
    )
  })
}
