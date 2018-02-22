library(leaflet)
library(shiny)
library(shinyjs)
library(sp)
library(raster)
library(rgdal)
library(RColorBrewer)


# setwd("~/Desktop/bioclimaticatlas")


data_species <- readRDS("data/species_list.rds")
data_climate <- readRDS("data/variables_list.rds")
grd     <- readRDS("data/grid.rds")



ui <- navbarPage(

  title       = "Tundra Nunavik",
  id          = "nav",
  collapsible = TRUE,


  ### HOME PANEL --------------------------------

  tabPanel(

    title = icon("home"),
    value = "tab_home",

    HTML("Work in progress...")
  ),


  ### CLIMATE CHANGE PANEL ----------------------

  tabPanel(

    title = "Climate change",
    value = "tab_climate",

    div(class = "outer",

      useShinyjs(),

      tags$head(
        includeCSS("css/style.css"),
        tags$link(
          rel  = "stylesheet",
          type = "text/css",
          href = "https://use.fontawesome.com/releases/v5.0.6/css/all.css"
        )
      ),

      leafletOutput(
        outputId = "map_climate",
        width    = "100%",
        height   = "100%"
      ),

      absolutePanel(
        id        = "panel_climate",
        class     = "panel panel-default",
        fixed     = TRUE,
        draggable = TRUE,
        top       = 60,
        left      = "auto",
        right     = 5,
        bottom    = "auto",
        width     = 340,
        height    = "auto",

        HTML('<h4>Climate change interface</h4><hr />'),

        radioButtons(
          inputId  = "language_climate",
          label    = "Select the language:",
          choices  = list("English" = "english", "French" = "french"),
          selected = "english",
          inline   = FALSE,
          width    = 300
        ),

        selectInput(
          inputId = "select_climate",
          label   = "Select the variable:",
          choices = c("Select a variable" = "", sort(unique(as.character(data_climate[, "english"]))))
        ),

        radioButtons(
          inputId  = "horizon_climate",
          label    = "Select the horizon:",
          choices  = c("1981-2010", "2011-2040", "2041-2070", "2071-2100"),
          selected = "1981-2010",
          inline   = FALSE,
          width    = 300
        ),

        radioButtons(
          inputId  = "scenario_climate",
          label    = "Select the RCP:",
          choices  = c("RCP4.5", "RCP8.5"),
          selected = character(0),
          inline   = FALSE,
          width    = 300
        ),

        radioButtons(
          inputId  = "infos_climate",
          label    = "Information to be displayed:",
          choices  = c("Climate normals", "Uncertainties", "Anomalies"),
          selected = "Climate normals",
          inline   = FALSE,
          width    = 300
        ),

        HTML(
          paste0(
            '<hr />',
            '<a href="#" id="btn-climate" class="btn-png">',
            '<i class="fa fa-download">',
            '</i>',
            'Download as PNG',
            '</a>'
          )
        )
      )
    )
  ),


  ### SPECIES DISTRIBUTION PANEL ----------------

  tabPanel(

    title = "Species distribution",
    value = "tab_species",

    div(class = "outer",

      leafletOutput(
        outputId = "map_species",
        width    = "100%",
        height   = "100%"
      ),

      absolutePanel(
        id        = "panel_species",
        class     = "panel panel-default",
        fixed     = TRUE,
        draggable = TRUE,
        top       = 60,
        left      = "auto",
        right     = 5,
        bottom    = "auto",
        width     = 340,
        height    = "auto",

        HTML('<h4>Species distribution interface</h4><hr />'),

        radioButtons(
          inputId  = "language_species",
          label    = "Search species by:",
          choices  = list("English name" = "common_en", "French name" = "common_fr", "Scientific name" = "latin", "Inuktitut name" = "inuktitut"),
          selected = "latin",
          inline   = FALSE,
          width    = 300
        ),

        radioButtons(
          inputId  = "class_species",
          label    = "Select the species:",
          choices  = list("Birds" = "Aves", "Mammals" = "Mammalia"),
          selected = "Aves",
          inline   = FALSE,
          width    = 300
        ),

        selectInput(
          inputId = "select_species",
          label   = NULL,
          choices = c("Select a species" = "", sort(unique(as.character(data_species[, "latin"]))))
        ),

        radioButtons(
          inputId  = "horizon_species",
          label    = "Select the horizon:",
          choices  = c("1981-2010", "2011-2040", "2041-2070", "2071-2100"),
          selected = "1981-2010",
          inline   = FALSE,
          width    = 300
        ),

        radioButtons(
          inputId  = "scenario_species",
          label    = "Select the RCP:",
          choices  = c("RCP4.5", "RCP8.5"),
          selected = character(0),
          inline   = FALSE,
          width    = 300
        ),

        radioButtons(
          inputId  = "infos_species",
          label    = "Information to be displayed:",
          choices  = c("Observations", "Binaries", "Probabilities", "Uncertainties"),
          selected = "Observations",
          inline   = FALSE,
          width    = 300
        ),

        HTML(
          paste0(
            '<hr />',
            '<a href="#" id="btn-species" class="btn-png">',
            '<i class="fa fa-download">',
            '</i>',
            'Download as PNG',
            '</a>'
          )
        )
      )
    )
  ),


  ### ECOSYSTEM CHANGES PANEL -------------------

  tabPanel(

    title = "Ecosystem changes",
    value = "tab_ecosystem",

    HTML("Work in progress...")
  ),


  ### Get Code PANEL -------------------------

  tabPanel(

    title = HTML(
      paste0(
        "<li class=\"github\">",
        "<a href=\"https://github.com/ahasverus/shiny-tundra-cc\" target=\"_blank\">",
        "<i class=\"fab fa-github\">",
        "</i>",
        "Get Code",
        "</a>",
        "</li>"
      )
    )
  )
)
