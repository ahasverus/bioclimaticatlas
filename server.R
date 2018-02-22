library(leaflet)
library(shiny)
library(shinyjs)
library(sp)
library(raster)
library(rgdal)
library(RColorBrewer)

# setwd("~/Desktop/")


data_species <- readRDS("data/species_list.rds")
for (i in 1:ncol(data_species)) data_species[ , i] <- as.character(data_species[ , i])
data_climate <- readRDS("data/variables_list.rds")
for (i in 1:ncol(data_climate)) data_climate[ , i] <- as.character(data_climate[ , i])
grd     <- readRDS("data/grid.rds")

server <- function(input, output, session) {


### BACKGROUND MAPS (ON APP OPENNING) -----------

  for (i in c("map_species", "map_climate")) {

    output[[i]] <- renderLeaflet({

      leaflet(data = grd) %>%

      addProviderTiles(provider = providers$Esri.WorldTopoMap) %>%

      addPolygons(
        color       = "#8e501d",
        weight      = 2,
        opacity     = 1.0,
        fillOpacity = 0,
        fillColor   = "transparent"
      ) %>%

      fitBounds(
        lng1 = ~-45.50,
        lat1 = ~ 51.00,
        lng2 = ~-75.50,
        lat2 = ~ 62.50
      ) %>%

      addEasyButton(
        easyButton(
          icon    = "fa-globe",
          title   = "Zoom to study area",
          onClick = JS(
            paste0("function(btn, ", i, "){ ", i, ".fitBounds([[51.00, -75.50], [62.50, -45.50]]); }")
          )
        )
      ) %>%

      addMiniMap(
        position      = "bottomleft",
        tiles         = providers$Esri.WorldTopoMap,
        toggleDisplay = TRUE
      )
    })
  }


  ### GET FORMS INPUT VALUES --------------------

  which_tab <- reactive({ input$nav })
  map_name  <- reactive({ gsub("tab_", "map_", which_tab()) })
  suffix    <- reactive({ gsub("tab_", "", which_tab()) })

  data      <- reactive({
    if (suffix() %in% c("species", "climate")) {
      if (suffix() == "species") { data_species }
      if (suffix() == "climate") { data_climate }
    } else {
      NULL
    }
  })

  langue    <- reactive({
    if (suffix() %in% c("species", "climate")) {
      input[[paste0("language_", suffix())]]
    } else {
      NULL
    }
  })

  spclass   <- reactive({
    if (suffix() == "species") {
      input[[paste0("class_", suffix())]]
    } else {
      NULL
    }
  })

  # observe({showNotification(paste0('"', spclass(), '"'))})

  var_list     <- reactive({
    if (suffix() %in% c("species", "climate")) {
      if (suffix() == "species") {
        c("Select a species"  = "", sort(as.character(data_species[which(data_species[ , "class"] == spclass()), langue()])))
      } else {
        c("Select a variable" = "", sort(as.character(data()[ , langue()])))
      }
    } else {
      NULL
    }
  })


  var_selected  <- reactive({

    if (suffix() %in% c("species", "climate")) {

      if (suffix() == "species") {

        if (input[[paste0("select_", suffix())]] == "") { # If no species selected (reset map)
          NULL
        } else {
          pos <- which(var_list() == input[[paste0("select_", suffix())]])
          if (length(pos) == 1) { # If same language and same class (update map)
            input[[paste0("select_", suffix())]]
          } else {
            pos <- which(data_species[ , langue()] == input[[paste0("select_", suffix())]])
            if (length(pos) == 1) { # If same language but different class (reset map)
              NULL
            } else {
              pos <- NULL
              for (i in c("common_en", "common_fr", "latin")) {
                if (length(which(data_species[ , i] == input[[paste0("select_", suffix())]])) > 0) {
                  pos <- which(data_species[ , i] == input[[paste0("select_", suffix())]])
                }
              }
              pos <- which(var_list() == data_species[pos, langue()])
              if (length(pos) == 1) { # Same species in a different language (same map)
                var_list()[pos]
              } else { # No correspondance in Inuktituk (reset map)
                NULL
              }
            }
          }
        }
      } else {
        if (input[[paste0("select_", suffix())]] == "") { # If no variable selected (reset map)
          NULL
        } else {
          pos <- which(var_list() == input[[paste0("select_", suffix())]])
          if (length(pos) == 1) { # If same language (update map)
            input[[paste0("select_", suffix())]]
          } else {
            pos <- which(data()[ , langue()] == input[[paste0("select_", suffix())]])
            if (length(pos) == 1) { # If same language but different class (reset map)
              NULL
            } else {
              pos <- NULL
              for (i in c("english", "french")) {
                if (length(which(data()[ , i] == input[[paste0("select_", suffix())]])) > 0) {
                  pos <- which(data()[ , i] == input[[paste0("select_", suffix())]])
                }
              }
              pos <- which(var_list() == data()[pos, langue()])
              if (length(pos) == 1) { # Same variable in a different language (same map)
                var_list()[pos]
              } else { # No correspondance in Inuktituk (reset map)
                NULL
              }
            }
          }
        }
      }
    } else {
      NULL
    }
  })

  period       <- reactive({
    if (suffix() %in% c("species", "climate")) {
      input[[paste0("horizon_", suffix())]]
    } else {
      NULL
    }
  })

  rcp          <- reactive({
    if (suffix() %in% c("species", "climate")) {
      if (period() == "1981-2010") {
        NULL
      } else {
        if (is.null(input[[paste0("scenario_", suffix())]])) {
          "RCP4.5"
        } else {
          input[[paste0("scenario_", suffix())]]
        }
      }
    } else {
      NULL
    }
  })

  information  <- reactive({
    if (suffix() %in% c("species", "climate")) {
      if (suffix() == "species") {
        if (period() != "1981-2010" && input[[paste0("infos_", suffix())]] == "Observations"){
          "Binaries"
        } else {
          input[[paste0("infos_", suffix())]]
        }
      } else {
        input[[paste0("infos_", suffix())]]
      }
    } else {
      NULL
    }
  })


  ### DEFINE REACTIVE EXPRESSIONS FOR MAP -------

  raster_path <- reactive({

    if (!is.null(var_selected())) {

      pos <- which(
        species[ , "common_en"] == var_selected() |
        species[ , "common_fr"] == var_selected() |
        species[ , "latin"]     == var_selected()
      )

      if (period() == "1981-2010"){
        paste0(
          "data/",
          tolower(spclass()),
          "/",
          species[pos, "code"],
          "_",
          period(),
          "_",
          tolower(information())
        )
      } else {
        paste0(
          "data/",
          tolower(spclass()),
          "/",
          species[pos, "code"],
          "_",
          period(),
          "_",
          tolower(gsub("\\.", "", rcp())),
          "_",
          tolower(information())
        )
      }
    } else {
      NULL
    }
  })

  ras <- reactive({

    if (!is.null(var_selected())) {
      readRDS(paste0(raster_path(), ".rds"))
    } else {
      NULL
    }
  })

  palette <- reactive({

    if (!is.null(var_selected())) {
      if (information() %in% c("Observations", "Binaries")){
        "FAC"
      } else {
        if (information() == "Probabilities"){
          "PRB"
        } else {
          "UNC"
        }
      }
    } else {
      NULL
    }
  })

  couleurs <- reactive({

    if (!is.null(var_selected())) {
      if (information() %in% c("Observations", "Binaries")){
        c("#E0E393", "#666131")
        # c("#FFFFCC", "#800026")
      } else {
        if (information() == "Probabilities"){
          colorBin(
            palette  = colorRampPalette(brewer.pal(n = 9, name = "YlOrRd"))(11),
            domain   = values(ras()),
            bin      = seq(0, 1, by = .10),
            na.color = "transparent"
          )
        } else {
          colorBin(
            palette  = colorRampPalette(brewer.pal(n = 9, name = "BuPu"))(11),
            domain   = values(ras()),
            bin      = seq(0, .5, by = .05),
            na.color = "transparent"
          )
        }
      }
    } else {
      NULL
    }
  })



### UPDATE FORMS INPUT CHOICES ----------------

  observe({

    if (suffix() %in% c("species", "climate")) {

      ### Update list of species / variables
      updateSelectInput(
        session  = session,
        inputId  = paste0("select_", suffix()),
        label    = NULL,
        choices  = var_list(),
        selected = ifelse(!is.null(var_selected()), var_selected(), "")
      )

      if (period() == "1981-2010") {

        ### Update RCP buttons
        shinyjs::disable(selector = "[type=radio][value='RCP4.5']")
        shinyjs::disable(selector = "[type=radio][value='RCP8.5']")
        shinyjs::runjs(paste0('$("#scenario_', suffix(), ' .shiny-options-group").css({"color": "#aaa"});'))
        shinyjs::runjs(paste0('$("#scenario_', suffix(), ' .shiny-options-group label").css({"cursor": "not-allowed"});'))
        shinyjs::runjs('$("[type=radio][value=\'RCP4.5\']").prop("checked", false);')
        shinyjs::runjs('$("[type=radio][value=\'RCP8.5\']").prop("checked", false);')

        ### Update INFORMATION button
        if (suffix() == "species") {
          shinyjs::enable(selector = "[type=radio][value='Observations']")
          shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:first-child").css({"color": "#555"});'))
          shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:first-child label").css({"cursor": "pointer"});'))
        }
      }

      if (period() != "1981-2010") {

        ### Update RCP buttons
        shinyjs::enable(selector = "[type=radio][value='RCP4.5']")
        shinyjs::enable(selector = "[type=radio][value='RCP8.5']")
        if (is.null(rcp())){
          updateRadioButtons(session, inputId = paste0("scenario_", suffix()), selected = "RCP4.5")
        } else {
          updateRadioButtons(session, inputId = paste0("scenario_", suffix()), selected = rcp())
        }
        shinyjs::runjs(paste0('$("#scenario_', suffix(), ' .shiny-options-group").css("color", "#555");'))
        shinyjs::runjs(paste0('$("#scenario_', suffix(), ' .shiny-options-group label").css({"cursor": "pointer"});'))

        ### Update INFORMATION button
        if (suffix() == "species") {
          shinyjs::disable(selector = "[type=radio][value='Observations']")
          shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:first-child").css({"color": "#aaa"});'))
          shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:first-child label").css({"cursor": "not-allowed"});'))
          updateRadioButtons(session, inputId = paste0("infos_", suffix()), selected = information())
        }
      }
    }
  })


### UPDATE SPECIES MAP ------------------------

  observe({

    if (suffix() %in% c("species", "climate")) {

      if (!is.null(var_selected())) {

        ### Update map
        leafletProxy(
          mapId = paste0("map_", suffix()),
          data  = grd) %>%
        clearShapes() %>%
        clearControls() %>%
        clearImages() %>%

        addRasterImage(
          x       = ras(),
          colors  = couleurs(),
          opacity = .75,
          project = FALSE
        ) %>%
        addPolygons(
          color       = "#8e501d",
          weight      = 2.0,
          opacity     = 1.0,
          fillOpacity = 0,
          fillColor   = "transparent"
        ) %>%
        fitBounds(
          lng1 = ~-45.50,
          lat1 = ~ 51.00,
          lng2 = ~-75.50,
          lat2 = ~ 62.50
        )

        ### Add legend
        if (palette() == "FAC"){
          leafletProxy(mapId = paste0("map_", suffix())) %>%
          addLegend(
            position  = "bottomleft",
            colors    = couleurs(),
            labels    = c("Absence", "Presence"),
            title     = "Map legend",
            opacity   = 1,
            className = "info legend"
          )
        }
        if (palette() == "PRB"){
          leafletProxy(mapId = paste0("map_", suffix())) %>%
          addLegend(
            position  = "bottomleft",
            pal       = couleurs(),
            values    = seq(0, 1, by = .1),
            title     = "Map legend",
            opacity   = 1,
            className = "info legend"
          )
        }
        if (palette() == "UNC"){
          leafletProxy(mapId = paste0("map_", suffix())) %>%
          addLegend(
            position  = "bottomleft",
            pal       = couleurs(),
            values    = seq(0, .5, by = .05),
            title     = "Map legend",
            opacity   = 1,
            className = "info legend"
          )
        }

      } else {

        ### Update map
        leafletProxy(
          mapId = paste0("map_", suffix()),
          data  = grd) %>%
        clearShapes() %>%
        clearControls() %>%
        clearImages() %>%

        addPolygons(
          color       = "#8e501d",
          weight      = 2.0,
          opacity     = 1.0,
          fillOpacity = 0,
          fillColor   = "transparent"
        ) %>%
        fitBounds(
          lng1 = ~-45.50,
          lat1 = ~ 51.00,
          lng2 = ~-75.50,
          lat2 = ~ 62.50
        )
      }
    }
  })


  onclick("nav", function() {

    for (i in c("map_climate", "map_species")) {

      leafletProxy(
        mapId = i,
        data  = grd
      ) %>%
      # clearShapes() %>%
      # clearControls() %>%
      # clearImages() %>%

      addPolygons(
        color       = "#8e501d",
        weight      = 2,
        opacity     = 1.0,
        fillOpacity = 0,
        fillColor   = "transparent"
      ) %>%

      fitBounds(
        lng1 = ~-45.50,
        lat1 = ~ 50.00,
        lng2 = ~-75.50,
        lat2 = ~ 63.00
      )
    }

    # updateSelectInput(
    #   session  = session,
    #   inputId  = 'select_species',
    #   label    = NULL,
    #   choices  = var_list(),
    #   selected = var_list()[1]
    # )
  })

  # observe({
  #   if (input$nav == "climate-change")
  #     showNotification(input$nav)
  # })
}
