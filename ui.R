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



ui <- navbarPage(

  title = "Tundra Nunavik",
  id    = "nav",
  collapsible = TRUE,

  tabPanel("Climate change",

    div(class = "outer",

      tags$head(
        includeCSS("css/style.css")
      ),
      tags$head(
        tags$link(
          rel  = "stylesheet",
          type = "text/css",
          href = "https://use.fontawesome.com/releases/v5.0.6/css/all.css"
        )
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput(outputId = "map", width = "100%", height = "100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(
        id        = "controls",
        class     = "panel panel-default",
        fixed     = TRUE,
        draggable = TRUE,
        top       = 60,
        left      = "auto",
        right     = 5,
        bottom    = "auto",
        width     = 340,
        height    = "auto",

        HTML('<h4>Navigation menu</h4>'),
        HTML('<hr />'),
        radioButtons(
          inputId  = "searchby",
          label    = "Search species by",
          choices  = list("English name" = "common_en", "French name" = "common_fr", "Scientific name" = "latin", "Inuktitut" = "inuktitut"),
          selected = "latin",
          inline   = FALSE,
          width    = 300
        ),
        radioButtons(
          inputId  = "spclass",
          label    = "Select the species",
          choices  = list("Birds" = "Aves", "Mammals" = "Mammalia"),
          selected = "Aves",
          inline   = FALSE,
          width    = 300
        ),
        selectInput(
          inputId = "species",
          label   = NULL,
          choices = sort(unique(as.character(species[, "latin"])))
        ),
        radioButtons(
          inputId  = "horizon",
          label    = "Select the horizon",
          choices  = c("1981-2010", "2011-2040", "2041-2070", "2071-2100"),
          selected = "1981-2010",
          inline   = FALSE,
          width    = 300
        ),
        radioButtons(
          inputId  = "scenario",
          label    = "Select the RCP",
          choices  = c("RCP4.5", "RCP8.5"),
          selected = character(0),
          inline   = FALSE,
          width    = 300
        ),
        radioButtons(
          inputId  = "infos",
          label    = "Information to be displayed",
          choices  = c("Observations", "Binaries", "Probabilities", "Uncertainties"),
          selected = "Observations",
          inline   = FALSE,
          width    = 300
        ),
        HTML('<hr />'),
          # a('Download as PNG', href = "#", icon = icon("download")),
        HTML('<a href="#" class="btn-png"><i class="fa fa-download"></i>Download as PNG</a>')#,
        # actionButton("xxx", 'Download as PNG', icon = icon("download"))
      )
    )
  ),
  tabPanel("Species distribution"),
  tabPanel("Ecosystem changes",
    HTML("Work in progress...")
  ),
  tabPanel("About",
    HTML("Work in progress...")
  )
)

#
# ui <- tagList(
#
#   includeCSS("css/style.css"),
#
#     navbarPage("Tundra Nunavik", id = "nav",                 # Brand title
#
#     tabPanel("Climate change",                               # Tab 1 title
#
#       titlePanel("Visualize climate change"),                # Page 1 title
#
#       column(width = 3,                                      # Form sidebar
#
#         wellPanel(
#
#           HTML('<h4>Navigation menu</h4>'),
#           # HTML('<hr /><div class="pad"></div>'),
#           HTML('<hr />'),
#           radioButtons(
#             inputId  = "searchby",
#             label    = "Search species by",
#             choices  = list("English name" = "common_en", "French name" = "common_fr", "Scientific name" = "latin"),
#             selected = "latin",
#             inline   = FALSE,
#             width    = 250
#           ),
#           radioButtons(
#             inputId  = "spclass",
#             label    = "Select the species",
#             choices  = list("Birds" = "Aves", "Mammals" = "Mammalia"),
#             selected = "Aves",
#             inline   = FALSE,
#             width    = 250
#           ),
#           selectInput(
#             inputId = "species",
#             label   = NULL,
#             choices = sort(unique(as.character(species[, "latin"])))
#           ),
#           radioButtons(
#             inputId  = "horizon",
#             label    = "Select the horizon",
#             choices  = c("1981-2010", "2011-2040", "2041-2070", "2071-2100"),
#             selected = "1981-2010",
#             inline   = FALSE,
#             width    = 250
#           ),
#           radioButtons(
#             inputId  = "scenario",
#             label    = "Select the RCP",
#             choices  = c("RCP4.5", "RCP8.5"),
#             selected = character(0),
#             inline   = FALSE,
#             width    = 250
#           ),
#           radioButtons(
#             inputId  = "infos",
#             label    = "Information to be displayed",
#             choices  = c("Observations", "Binaries", "Probabilities", "Uncertainties"),
#             selected = "Observations",
#             inline   = FALSE,
#             width    = 250
#           )#,
#           # HTML('<hr /><div class="pad"></div>'),
#         )
#       ),
#
#       column(width = 9,
#
#         leafletOutput(
#           outputId = "map",
#           # width    = "98.0%",
#           height   = "560"
#         )
#       )
#     )
#   )
# )
