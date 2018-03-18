library(leaflet)
library(shiny)
library(shinyjs)
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(Cairo)
library(RColorBrewer)



### LOAD ADDING FUNCTIONS ------------------------------------------------------

fls <- list.files(
  path       = "R",
  pattern    = "\\.R$",
  full.names = TRUE
)
tmp <- sapply(fls, source, .GlobalEnv)
rm(list = c("fls", "tmp"))


### LOAD DATASETS --------------------------------------------------------------

data_species       <- readRDS("data/infos/species_list.rds")
data_climate       <- readRDS("data/infos/variables_list.rds")
data_ecosystem     <- readRDS("data/infos/ecosystem_list.rds")
data_network       <- readRDS("data/infos/network_list.rds")
data_vulnerability <- data_network[which(data_network[ , "code"] == "Net01"), ]
data_network       <- data_network[which(data_network[ , "code"] != "Net01"), ]
grd                <- readRDS("data/background/grid.rds")
grd_tundra         <- readRDS("data/background/grid_tundra.rds")


### AVAILABLE RAMP COLORS ------------------------------------------------------

rampcolors <- data.frame(
  palette          = rownames(brewer.pal.info),
  maxcolors        = brewer.pal.info[ , "maxcolors"],
  stringsAsFactors = FALSE
)



ui <- navbarPage(

  title       = "Tundra Nunavik",
  id          = "nav",
  collapsible = TRUE,


  ### HOME PANEL ---------------------------------------------------------------

  tabPanel(

    title = icon("home"),
    value = "tab_home",

    includeHTML("includes/home-page.html")
  ),


  ### CLIMATE CHANGE PANEL -----------------------------------------------------

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
          choices  = list(
            "English" = "english",
            "French"  = "french"
          ),
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
          choices  = list(
            "RCP4.5" = "rcp45_climate",
            "RCP8.5" = "rcp85_climate"
          ),
          selected = character(0),
          inline   = FALSE,
          width    = 300
        ),

        radioButtons(
          inputId  = "infos_climate",
          label    = "Information to be displayed:",
          choices  = list(
            "Climate normals" = "normals_climate",
            "Uncertainties"   = "uncertainties_climate",
            "Anomalies"       = "anomalies_climate"
          ),
          selected = "normals_climate",
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

        HTML(
          paste0(
            '<hr />',
            '<div class="buttons">',
            '<div id="btn-climate" class="btn-png btn-left">',
            '<i class="fa fa-download"></i>',
            'Download Map',
            '</div>',
            '<div id="help-climate" class="btn-png btn-right">',
            '<i class="fa fa-info-circle"></i>',
            'Informations',
            '</div>',
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
          choices  = list(
            "English name"    = "common_en",
            "French name"     = "common_fr",
            "Scientific name" = "latin",
            "Inuktitut name"  = "inuktitut"
          ),
          selected = "latin",
          inline   = FALSE,
          width    = 300
        ),

        radioButtons(
          inputId  = "class_species",
          label    = "Select the species:",
          choices  = list(
            "Birds"   = "Aves",
            "Mammals" = "Mammalia"
          ),
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
          choices  = list(
            "RCP4.5" = "rcp45_species",
            "RCP8.5" = "rcp85_species"
          ),
          selected = character(0),
          inline   = FALSE,
          width    = 300
        ),

        radioButtons(
          inputId  = "infos_species",
          label    = "Information to be displayed:",
          choices  = list(
            "Observations"  = "observations_species",
            "Binaries"      = "binaries_species",
            "Probabilities" = "probabilities_species",
            "Uncertainties" = "uncertainties_species"
          ),
          selected = "observations_species",
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
            '<div class="buttons">',
            '<div id="btn-species" class="btn-png btn-left">',
            '<i class="fa fa-download"></i>',
            'Download Map',
            '</div>',
            '<div id="help-species" class="btn-png btn-right">',
            '<i class="fa fa-info-circle"></i>',
            'Informations',
            '</div>',
            '</div>'
          )
        )
      )
    )
  ),


  ### BIODIVERSITY CHANGES PANEL -----------------------------------------------

  tabPanel(

    title = "Biodiversity distribution",
    value = "tab_ecosystem",

    div(class = "outer",

      leafletOutput(
        outputId = "map_ecosystem",
        width    = "100%",
        height   = "100%"
      ),

      absolutePanel(
        id        = "panel_ecosystem",
        class     = "panel panel-default",
        fixed     = TRUE,
        draggable = TRUE,
        top       = 60,
        left      = "auto",
        right     = 5,
        bottom    = "auto",
        width     = 340,
        height    = "auto",

        HTML('<h4>Biodiversity interface</h4><hr />'),

        radioButtons(
          inputId  = "class_ecosystem",
          label    = "Select the species group:",
          choices  = list(
            "All species" = "total",
            "Birds"       = "birds",
            "Mammals"     = "mammals"
          ),
          selected = "total",
          inline   = FALSE,
          width    = 300
        ),

        radioButtons(
          inputId  = "horizon_ecosystem",
          label    = "Select the horizon:",
          choices  = c("1981-2010", "2011-2040", "2041-2070", "2071-2100"),
          selected = "1981-2010",
          inline   = FALSE,
          width    = 300
        ),

        radioButtons(
          inputId  = "scenario_ecosystem",
          label    = "Select the RCP:",
          choices  = list(
            "RCP4.5" = "rcp45_ecosystem",
            "RCP8.5" = "rcp85_ecosystem"
          ),
          selected = character(0),
          inline   = FALSE,
          width    = 300
        ),

        radioButtons(
          inputId  = "infos_ecosystem",
          label    = "Information to be displayed:",
          choices  = c(
            "Species richness" = "richness",
            "Species gains"    = "gains",
            "Species losses"   = "losses",
            "Turnover"         = "turnover"
          ),
          selected = "richness",
          inline   = FALSE,
          width    = 300
        ),

        HTML(
          paste0(
            "<h5>Select a color palette:</h5>",
            "<div class=\"color-picker\">",
            "  <div id=\"grad_ecosystem\">",
            "    <div id=\"color-sel_ecosystem\" class=\"YlGnBu\"></div>",
            "    <div id=\"color-arrow_ecosystem\">",
            "      <i class=\"fa fa-caret-down\"></i>",
            "    </div>",
            "  </div>",
            "  <div id=\"menu_ecosystem\">"
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
          inputId = "color_ecosystem",
          label   = "",
          value   = ""
        ),

        HTML(
          paste0(
            '<hr />',
            '<div class="buttons">',
            '<div id="btn-ecosystem" class="btn-png btn-left">',
            '<i class="fa fa-download"></i>',
            'Download Map',
            '</div>',
            '<div id="help-ecosystem" class="btn-png btn-right">',
            '<i class="fa fa-info-circle"></i>',
            'Informations',
            '</div>',
            '</div>'
          )
        )
      )
    )
  ),


  ### NETWORK CHANGES PANEL ----------------------------------------------------

  tabPanel(

    title = "Trophic network",
    value = "tab_network",

    div(class = "outer",

      leafletOutput(
        outputId = "map_network",
        width    = "100%",
        height   = "100%"
      ),

      absolutePanel(
        id        = "panel_network",
        class     = "panel panel-default",
        fixed     = TRUE,
        draggable = TRUE,
        top       = 60,
        left      = "auto",
        right     = 5,
        bottom    = "auto",
        width     = 340,
        height    = "auto",

        HTML('<h4>Trophic network interface</h4><hr />'),

        radioButtons(
          inputId  = "language_network",
          label    = "Select the language:",
          choices  = list(
            "English" = "english",
            "French"  = "french"
          ),
          selected = "english",
          inline   = FALSE,
          width    = 300
        ),

        selectInput(
          inputId = "select_network",
          label   = "Select the variable:",
          choices = c("Select a variable" = "", sort(unique(as.character(data_network[, "english"]))))
        ),

        radioButtons(
          inputId  = "horizon_network",
          label    = "Select the horizon:",
          choices  = c("1981-2010", "2011-2040", "2041-2070", "2071-2100"),
          selected = "1981-2010",
          inline   = FALSE,
          width    = 300
        ),

        radioButtons(
          inputId  = "scenario_network",
          label    = "Select the RCP:",
          choices  = list(
            "RCP4.5" = "rcp45_network",
            "RCP8.5" = "rcp85_network"
          ),
          selected = character(0),
          inline   = FALSE,
          width    = 300
        ),

        radioButtons(
          inputId  = "infos_network",
          label    = "Information to be displayed:",
          choices  = list(
            "Values"    = "values_network",
            "Anomalies" = "anomalies_network"
          ),
          selected = "values_network",
          inline   = FALSE,
          width    = 300
        ),

        HTML(
          paste0(
            "<h5>Select a color palette:</h5>",
            "<div class=\"color-picker\">",
            "  <div id=\"grad_network\">",
            "    <div id=\"color-sel_network\" class=\"YlGnBu\"></div>",
            "    <div id=\"color-arrow_network\">",
            "      <i class=\"fa fa-caret-down\"></i>",
            "    </div>",
            "  </div>",
            "  <div id=\"menu_network\">"
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
          inputId = "color_network",
          label   = "",
          value   = ""
        ),

        HTML(
          paste0(
            '<hr />',
            '<div class="buttons">',
            '<div id="btn-network" class="btn-png btn-left">',
            '<i class="fa fa-download"></i>',
            'Download Map',
            '</div>',
            '<div id="help-network" class="btn-png btn-right">',
            '<i class="fa fa-info-circle"></i>',
            'Informations',
            '</div>',
            '</div>'
          )
        )
      )
    )
  ),


  ### NETWORK CHANGES PANEL ----------------------------------------------------

  tabPanel(

    title = "Vulnerability index",
    value = "tab_vulnerability",

    div(class = "outer",

      leafletOutput(
        outputId = "map_vulnerability",
        width    = "100%",
        height   = "100%"
      ),

      absolutePanel(
        id        = "panel_vulnerability",
        class     = "panel panel-default",
        fixed     = TRUE,
        draggable = TRUE,
        top       = 60,
        left      = "auto",
        right     = 5,
        bottom    = "auto",
        width     = 340,
        height    = "auto",

        HTML('<h4>Vulnerability index</h4><hr />'),

        radioButtons(
          inputId  = "horizon_vulnerability",
          label    = "Select the horizon:",
          choices  = c("2011-2040", "2041-2070", "2071-2100"),
          selected = "2011-2040",
          inline   = FALSE,
          width    = 300
        ),

        radioButtons(
          inputId  = "scenario_vulnerability",
          label    = "Select the RCP:",
          choices  = list(
            "RCP4.5" = "rcp45_vulnerability",
            "RCP8.5" = "rcp85_vulnerability"
          ),
          selected = character(0),
          inline   = FALSE,
          width    = 300
        ),

        HTML(
          paste0(
            "<h5>Select a color palette:</h5>",
            "<div class=\"color-picker\">",
            "  <div id=\"grad_vulnerability\">",
            "    <div id=\"color-sel_vulnerability\" class=\"YlGnBu\"></div>",
            "    <div id=\"color-arrow_vulnerability\">",
            "      <i class=\"fa fa-caret-down\"></i>",
            "    </div>",
            "  </div>",
            "  <div id=\"menu_vulnerability\">"
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
          inputId = "color_vulnerability",
          label   = "",
          value   = ""
        ),

        HTML(
          paste0(
            '<hr />',
            '<div class="buttons">',
            '<div id="btn-vulnerability" class="btn-png btn-left">',
            '<i class="fa fa-download"></i>',
            'Download Map',
            '</div>',
            '<div id="help-vulnerability" class="btn-png btn-right">',
            '<i class="fa fa-info-circle"></i>',
            'Informations',
            '</div>',
            '</div>'
          )
        )
      )
    )
  )

  ### GET CODE PANEL -----------------------------------------------------------

  # Added with jQuery

)
