library(leaflet)
library(shiny)
library(shinyjs)
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(Cairo)
library(png)
library(RColorBrewer)


fls <- list.files(path = "R", pattern = "\\.R$", full.names = TRUE)
tmp <- sapply(fls, source, .GlobalEnv)
rm(list = c("fls", "tmp"))


data_species <- readRDS("data/infos/species_list.rds")
data_climate <- readRDS("data/infos/variables_list.rds")
grd          <- readRDS("data/background/grid.rds")


rampcolors <- data.frame(
  palette          = rownames(brewer.pal.info),
  maxcolors        = brewer.pal.info[ , "maxcolors"],
  stringsAsFactors = FALSE
)



ui <- navbarPage(

  title       = "Tundra Nunavik",
  id          = "nav",
  collapsible = TRUE,


  ### HOME PANEL --------------------------------

  tabPanel(

    title = icon("home"),
    value = "tab_home",

    HTML("Coming soon...")
  ),


  ### CLIMATE CHANGE PANEL ----------------------

  tabPanel(

    title = "Climate change",
    value = "tab_climate",

    div(class = "outer",

      useShinyjs(),

      tags$head(
        includeCSS("css/style.css"),
        includeCSS("css/color-gradients.css"),
        tags$link(
          rel  = "stylesheet",
          type = "text/css",
          href = "https://use.fontawesome.com/releases/v5.0.6/css/all.css"
        ),
        includeCSS("css/font-awesome-animation.min.css"),
        includeScript("js/appscript.js")
        # tags$script(src = "js/appscript.js")
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
            "<h5>Select a color palette:</h5>",
            "<div class=\"color-picker\">",
            "  <div id=\"grad_climate\">",
            "    <div id=\"color-sel_climate\" class=\"YlGnBu\"></div>",
            "    <div id=\"color-arrow_climate\">",
            "      <i class=\"fa fa-caret-down\"></i>",
            "    </div>",
            "  </div>",
            "  <div id=\"menu_climate\">"
          )
        ),

        includeHTML("includes/color-picker.html"),

        HTML(
          paste0(
          "  </div>",
          "</div>"
          )
        ),

        textInput(
          inputId = "color_climate",
          label   = "",
          value   = ""
        ),

        # HTML("<div class=\"helper\">"),
        # HTML("<div class=\"help-left\">"),
        # checkboxInput("fixed_climate", "Fixed color scale", FALSE),
        # HTML("</div>"),
        # HTML("<div class=\"help-right\">"),
        # HTML("<i class=\"fa fa-info-circle\" id=\"help_fixed_climate\"></i>"),
        # HTML("</div>"),
        # HTML("</div>"),

        HTML(
          paste0(
            '<hr />',
            '<div id="btn-climate" class="btn-png">',
            '<i class="fa fa-download"></i>',
            'Download Map',
            '</div>'
          )
        )
      )
    )
  ),


  ### SPECIES DISTRIBUTION PANEL -----------------------------------------------

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
            "<h5>Select a color palette:</h5>",
            "<div class=\"color-picker\">",
            "  <div id=\"grad_species\">",
            "    <div id=\"color-sel_species\" class=\"YlGnBu\"></div>",
            "    <div id=\"color-arrow_species\">",
            "      <i class=\"fa fa-caret-down\"></i>",
            "    </div>",
            "  </div>",
            "  <div id=\"menu_species\">"
          )
        ),

        includeHTML("includes/color-picker.html"),

        HTML(
          paste0(
          "  </div>",
          "</div>"
          )
        ),

        textInput(
          inputId = "color_species",
          label   = "",
          value   = ""
        ),

        HTML(
          paste0(
            '<hr />',
            '<div id="btn-species" class="btn-png">',
            '<i class="fa fa-download"></i>',
            'Download Map',
            '</div>'
          )
        )
      )
    )
  ),


  ### ECOSYSTEM CHANGES PANEL -------------------

  tabPanel(

    title = "Ecosystem changes",
    value = "tab_ecosystem",

    HTML("Coming soon...")
  )


  ### Get Code PANEL -------------------------

  # Add with jQuery
)
