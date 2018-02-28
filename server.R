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

data_species <- readRDS("data/infos/species_list.rds")
data_climate <- readRDS("data/infos/variables_list.rds")
grd          <- readRDS("data/background/grid.rds")


### AVAILABLE RAMP COLORS ------------------------------------------------------

rampcolors <- data.frame(
  palette          = rownames(brewer.pal.info),
  maxcolors        = brewer.pal.info[ , "maxcolors"],
  stringsAsFactors = FALSE
)


### CAIRO FONTS ----------------------------------------------------------------

cairoFont <- "Times New Roman"
CairoFonts(
  regular    = paste0(cairoFont, ":style=Regular"),
  bold       = paste0(cairoFont, ":style=Bold"),
  italic     = paste0(cairoFont, ":style=Italic"),
  bolditalic = paste0(cairoFont, ":style=Bold Italic,BoldItalic")
)



server <- function(input, output, session) {


### BACKGROUND MAPS (ON APP OPENNING) ------------------------------------------

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


  ### CATCH SHINY INPUT VALUES -------------------------------------------------

  which_tab <- reactive({ input$nav })
  map_name  <- reactive({ gsub("tab_", "map_", which_tab()) })
  suffix    <- reactive({ gsub("tab_", "", which_tab()) })

  data <- reactive({

    if (suffix() %in% c("species", "climate")) {
      if (suffix() == "species") { data_species }
      if (suffix() == "climate") { data_climate }
    } else {
      NULL
    }
  })

  langue <- reactive({

    if (suffix() %in% c("species", "climate")) {
      input[[paste0("language_", suffix())]]
    } else {
      NULL
    }
  })

  spclass <- reactive({

    if (suffix() == "species") {
      input[[paste0("class_", suffix())]]
    } else {
      NULL
    }
  })

  var_list <- reactive({

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


  var_selected <- reactive({

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
              for (i in c("common_en", "common_fr", "latin", "inuktitut")) {
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

  period <- reactive({

    if (suffix() %in% c("species", "climate")) {
      input[[paste0("horizon_", suffix())]]
    } else {
      NULL
    }
  })

  rcp <- reactive({

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

  var_english  <- reactive({

    if (!is.null(var_selected())) {

      if (suffix() == "species") {

        pos <- which(
          data_species[ , "common_en"] == var_selected() |
          data_species[ , "common_fr"] == var_selected() |
          data_species[ , "latin"]     == var_selected() |
          data_species[ , "inuktitut"] == var_selected()
        )
        as.character(data_species[pos, "common_en"])
      }

      if (suffix() == "climate") {

        pos <- which(
          data_climate[ , "english"] == var_selected() |
          data_climate[ , "french"]  == var_selected()
        )
        as.character(data_climate[pos, "english"])
      }

    } else {
      NULL
    }
  })

  var_units  <- reactive({

    if (!is.null(var_selected())) {
      if (suffix() == "climate") {
        pos <- which(
          data_climate[ , "english"] == var_selected() |
          data_climate[ , "french"] == var_selected()
        )
        unite <- as.character(data_climate[pos, "units"])
        if (unite == "-") {
          NULL
        } else {
          as.character(data_climate[pos, "units"])
        }
      } else {
        NULL
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
        if (period() == "1981-2010" && input[[paste0("infos_", suffix())]] %in% c("Anomalies", "Uncertainties")){
          "Climate normals"
        } else {
          input[[paste0("infos_", suffix())]]
        }
      }
    } else {
      NULL
    }
  })

  couleur <- reactive({ input[[paste0("color_", suffix())]] })
  dpi     <- reactive({ as.numeric(input[[paste0("dpi_", suffix())]]) })
  type    <- reactive({ input[[paste0("format_", suffix())]] })



  ### CREATE RASTER PATH -------------------------------------------------------

  raster_path <- reactive({

    if (!is.null(var_selected())) {

      if (suffix() == "species") {

        pos <- which(
          data_species[ , "common_en"] == var_selected() |
          data_species[ , "common_fr"] == var_selected() |
          data_species[ , "latin"]     == var_selected() |
          data_species[ , "inuktitut"] == var_selected()
        )

        if (period() == "1981-2010"){
          paste0(
            "data/",
            tolower(spclass()),
            "/",
            data_species[pos, "code"],
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
            data_species[pos, "code"],
            "_",
            period(),
            "_",
            tolower(gsub("\\.", "", rcp())),
            "_",
            tolower(information())
          )
        }
      } else {
        if (suffix() == "climate") {

          pos <- which(
            data_climate[ , "english"] == var_selected() |
            data_climate[ , "french"]  == var_selected()
          )

          if (period() == "1981-2010"){
            paste0(
              "data/climate/",
              data_climate[pos, "code"],
              "_",
              gsub("-", "", period()),
              "_",
              ifelse(
                information() == "Climate normals",
                "mean",
                ifelse(
                  information() == "Uncertainties",
                  "sd",
                  "diff"
                )
              )
            )
          } else {
            paste0(
              "data/climate/",
              data_climate[pos, "code"],
              "_",
              gsub("-", "", period()),
              "_",
              tolower(gsub("\\.", "", rcp())),
              "_",
              ifelse(
                information() == "Climate normals",
                "mean",
                ifelse(
                  information() == "Uncertainties",
                  "sd",
                  "diff"
                )
              )
            )
          }
        }
      }
    } else {
      NULL
    }
  })


  ### LOAD RASTER --------------------------------------------------------------

  ras <- reactive({

    if (!is.null(var_selected())) {
      ras <- readRDS(paste0(raster_path(), ".rds"))
      if (information() == "Probabilities") {
        ras[][which(!is.na(ras[]))] <- ifelse(ras[][which(!is.na(ras[]))] > 1, 1, ras[][which(!is.na(ras[]))])
      }
      ras
    } else {
      NULL
    }
  })


  ### CREATE COLOR PALETTE (based on data) -------------------------------------

  couleurs <- reactive({

    if (!is.null(var_selected())) {

      mycol <- brewer.pal(
        n    = rampcolors[which(rampcolors[ , "palette"] == gsub("-rev", "", couleur())), "maxcolors"],
        name = gsub("-rev", "", couleur())
      )
      if (length(grep("-rev", couleur())) == 1) { # If reverse palette
        mycol <- mycol[length(mycol):1]
      }

      if (length(unique(values(ras()))) > 2) { # More than 1 single value
        colorNumeric(palette = mycol, domain = values(ras()), na.color = "transparent")
      } else {
        if (length(which(unique(values(ras())) == 0) == 1)) {
          colorNumeric(palette = mycol[1], domain = values(ras()), na.color = "transparent")
        } else {
          colorNumeric(palette = mycol[length(mycol)], domain = values(ras()), na.color = "transparent")
        }
      }
    } else {
      NULL
    }
  })


  observe({

    if (suffix() %in% c("species", "climate")) {

      ### UPDATE LIST OF SPECIES / VARIABLES -----------------------------------

      updateSelectInput(
        session  = session,
        inputId  = paste0("select_", suffix()),
        label    = NULL,
        choices  = var_list(),
        selected = ifelse(!is.null(var_selected()), var_selected(), "")
      )

      if (period() == "1981-2010") {

        ### DISABLE RCP RADIO --------------------------------------------------

        shinyjs::disable(selector = "[type=radio][value='RCP4.5']")
        shinyjs::disable(selector = "[type=radio][value='RCP8.5']")
        shinyjs::runjs(paste0('$("#scenario_', suffix(), ' .shiny-options-group").css({"color": "#aaa"});'))
        shinyjs::runjs(paste0('$("#scenario_', suffix(), ' .shiny-options-group label").css({"cursor": "not-allowed"});'))
        shinyjs::runjs('$("[type=radio][value=\'RCP4.5\']").prop("checked", false);')
        shinyjs::runjs('$("[type=radio][value=\'RCP8.5\']").prop("checked", false);')


        ### ENABLE OBSERVATION RADIO -------------------------------------------

        if (suffix() == "species") {
          shinyjs::enable(selector = "[type=radio][value='Observations']")
          shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:first-child").css({"color": "#555"});'))
          shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:first-child label").css({"cursor": "pointer"});'))
        }


        ### DISABLE ANOMALIES & UNCERTAINTIES ----------------------------------

        if (suffix() == "climate") {
          shinyjs::disable(selector = "[type=radio][value='Anomalies']")
          shinyjs::disable(selector = "[type=radio][value='Uncertainties']")
          shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:nth-child(2)").css({"color": "#aaa"});'))
          shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:nth-child(2) label").css({"cursor": "not-allowed"});'))
          shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:nth-child(3)").css({"color": "#aaa"});'))
          shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:nth-child(3) label").css({"cursor": "not-allowed"});'))
          shinyjs::runjs('$("[type=radio][value=\'Anomalies\']").prop("checked", false);')
          shinyjs::runjs('$("[type=radio][value=\'Uncertainties\']").prop("checked", false);')
          updateRadioButtons(session, inputId = paste0("infos_", suffix()), selected = information())
        }
      }

      if (period() != "1981-2010") {


        ### ENABLE RCP RADIO ---------------------------------------------------

        shinyjs::enable(selector = "[type=radio][value='RCP4.5']")
        shinyjs::enable(selector = "[type=radio][value='RCP8.5']")
        if (is.null(rcp())){
          updateRadioButtons(session, inputId = paste0("scenario_", suffix()), selected = "RCP4.5")
        } else {
          updateRadioButtons(session, inputId = paste0("scenario_", suffix()), selected = rcp())
        }
        shinyjs::runjs(paste0('$("#scenario_', suffix(), ' .shiny-options-group").css("color", "#555");'))
        shinyjs::runjs(paste0('$("#scenario_', suffix(), ' .shiny-options-group label").css({"cursor": "pointer"});'))


        ### DISABLE OBSERVATIONS RADIO -----------------------------------------

        if (suffix() == "species") {
          shinyjs::disable(selector = "[type=radio][value='Observations']")
          shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:first-child").css({"color": "#aaa"});'))
          shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:first-child label").css({"cursor": "not-allowed"});'))
          updateRadioButtons(session, inputId = paste0("infos_", suffix()), selected = information())
        }


        ### ENABLE ANOMALIES & UNCERTAINTIES -----------------------------------

        if (suffix() == "climate") {
          shinyjs::enable(selector = "[type=radio][value='Anomalies']")
          shinyjs::enable(selector = "[type=radio][value='Uncertainties']")
          shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:nth-child(2)").css({"color": "#555"});'))
          shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:nth-child(2) label").css({"cursor": "pointer"});'))
          shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:nth-child(3)").css({"color": "#555"});'))
          shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:nth-child(3) label").css({"cursor": "pointer"});'))
        }
      }


      ### ENABLE / DISABLE DPI OPTION ------------------------------------------

      if (!is.null(type())) {
        if (type() == "PDF") {
          shinyjs::disable(id = paste0("dpi_", suffix()))
        } else {
          shinyjs::enable(id = paste0("dpi_", suffix()))
        }
      }
    }
  })


### UPDATE / RESET MAP(S) --------------------------------------------------------

  observe({

    if (suffix() %in% c("species", "climate")) {

      if (!is.null(var_selected())) {


        ### UPDATE MAP ---------------------------------------------------------

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


        ### ADD MAP LEGEND -----------------------------------------------------

        leafletProxy(mapId = paste0("map_", suffix())) %>%
        addLegend(
          position  = "bottomleft",
          pal       = couleurs(),
          values    = ras()[],
          title     = information(),
          opacity   = 1,
          className = "info legend"
        )

      } else {


        ### RESET MAP ----------------------------------------------------------

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


  ### (USELESS ????) -----------------------------------------------------------

  # onclick("nav", function() {
  #
  #   for (i in c("map_climate", "map_species")) {
  #
  #     leafletProxy(
  #       mapId = i,
  #       data  = grd
  #     ) %>%
  #
  #     addPolygons(
  #       color       = "#8e501d",
  #       weight      = 2,
  #       opacity     = 1.0,
  #       fillOpacity = 0,
  #       fillColor   = "transparent"
  #     ) %>%
  #
  #     fitBounds(
  #       lng1 = ~-45.50,
  #       lat1 = ~ 50.00,
  #       lng2 = ~-75.50,
  #       lat2 = ~ 63.00
  #     )
  #   }
  # })


  ### HIDE COLOR PALETTES ON OPENNING ------------------------------------------

  observe({ hide(id = paste0("color_", suffix())) })


  ### SHOW / HIDE COLOR PALETTES -----------------------------------------------

  onclick("grad_species", function(){
    toggle(id = "menu_species")
    toggleClass(id = "grad_species", class = "shadow")
  })

  onclick("grad_climate", function(){
    toggle(id = "menu_climate")
    toggleClass(id = "grad_climate", class = "shadow")
  })

  onclick("menu_species", function(){
    toggle(id = "menu_species")
    toggleClass(id = "grad_species", class = "shadow")
  })

  onclick("menu_climate", function(){
    toggle(id = "menu_climate")
    toggleClass(id = "grad_climate", class = "shadow")
  })


  ### CLICK ON DOWNLOAD BUTTON (SPECIES) ---------------------------------------

  onclick("btn-species", function(){


    ### SHOW DOWNLOAD PARAMETERS -----------------------------------------------

    if (!is.null(var_selected())) {

      showModal(
        modalDialog(

          title = "Download options",

          HTML("<div id=\"save_species\">"),

          fluidRow(

            column(6,
              radioButtons(
                inputId  = "format_species",
                label    = "Output format:",
                choices  = c("PNG", "JPEG", "TIFF", "PDF"),
                selected = "PNG",
                inline   = FALSE
              )
            ),

            column(6,
              selectInput(
                inputId  = "dpi_species",
                label    = "Resolution (dpi):",
                choices  = c(72, 96, 150, 300, 600, 900),
                selected = 300,
                width    = 150
              )
            )
          ),

          HTML("<div class=\"bouton\">"),
          downloadButton("download_species", "Download map"),
          HTML("<br /></div></div>"),

          easyClose = TRUE,
          footer    = NULL
        )
      )
    } else {


      ### ERROR MESSAGE (no species selected) ----------------------------------

      showModal(
        modalDialog(
          title = "Warning!",
          HTML(
            paste0(
              "<div class=\"msg\">",
              "Please select a species.",
              "<br /><br />",
              "<i class=\"fas fa-times-circle fa-4x\"></i>",
              "<br /><br />",
              "</div>"
            )
          ),
          easyClose = TRUE,
          footer    = NULL
        )
      )
    }
  })


  ### CLICK ON DOWNLOAD BUTTON (CLIMATE) ---------------------------------------

  onclick("btn-climate", function(){


    ### SHOW DOWNLOAD PARAMETERS -----------------------------------------------

    if (!is.null(var_selected())) {

      showModal(
        modalDialog(

          title = "Download options",

          HTML("<div id=\"save_climate\">"),

          fluidRow(

            column(6,
              radioButtons(
                inputId  = "format_climate",
                label    = "Output format:",
                choices  = c("PNG", "JPEG", "TIFF", "PDF"),
                selected = "PNG",
                inline   = FALSE
              )
            ),

            column(6,
              selectInput(
                inputId  = "dpi_climate",
                label    = "Resolution (dpi):",
                choices  = c(72, 96, 150, 300, 600, 900),
                selected = 300,
                width    = 150
              )
            )
          ),

          HTML("<div class=\"bouton\">"),
          downloadButton("download_climate", "Download map"),
          HTML("<br /></div></div>"),

          easyClose = TRUE,
          footer    = NULL
        )
      )
    } else {


      ### ERROR MESSAGE (no variable selected) ---------------------------------

      showModal(
        modalDialog(

          title = "Warning!",

          HTML(
            paste0(
              "<div class=\"msg\">",
              "Please select a variable.",
              "<br /><br />",
              "<i class=\"fas fa-times-circle fa-4x\"></i>",
              "<br /><br />",
              "</div>"
            )
          ),
          easyClose = TRUE,
          footer    = NULL
        )
      )
    }
  })


  ### CLICK ON POPUP DOWNLOAD BUTTON (SPECIES) ---------------------------------

  output$download_species <- downloadHandler(

    filename =  function() {


      ### EXPORTED MAP NAME ----------------------------------------------------

      paste0(
        "map-",
        gsub("[[:punct:]]|[[:space:]]", "", Sys.time()),
        ".",
        tolower(type())
      )
    },

    content  = function(file) {


      ### MESSAGE DURING MAP PREPARATION ---------------------------------------

      showModal(
        modalDialog(

          title = "Message",

          HTML(
            paste0(
              "<div class=\"msg\">",
              "Your map is being prepared.",
              "<br />",
              "Please wait a few seconds...",
              "<br /><br />",
              "<i class=\"fas fa-spinner faa-spin animated fa-4x\"></i>",
              "<br /><br />",
              "</div>"
            )
          ),
          easyClose = FALSE,
          footer    = NULL
        )
      )


      ### DEVISE INITIALISATION ------------------------------------------------

      if (type() == "PNG") {
        CairoPNG(
          filename  = file,
          width     = 5.00 * 1.2,
          height    = 5.80 * 1.2,
          pointsize = 14,
          bg        = "transparent",
          units     = "in",
          res       = dpi()
        )
      }

      if (type() == "JPEG") {
        CairoJPEG(
          filename  = file,
          width     = 5.00 * 1.2,
          height    = 5.80 * 1.2,
          pointsize = 14,
          bg        = "transparent",
          units     = "in",
          res       = dpi()
        )
      }

      if (type() == "TIFF") {
        CairoTIFF(
          filename  = file,
          width     = 5.00 * 1.2,
          height    = 5.80 * 1.2,
          pointsize = 14,
          bg        = "transparent",
          units     = "in",
          res       = dpi()
        )
      }

      if (type() == "PDF") {
        CairoPDF(
          file      = file,
          width     = 5.00 * 1.2,
          height    = 5.80 * 1.2,
          pointsize = 14,
          bg        = "transparent"
        )
      }


      ### MAP INFORMATIONS -----------------------------------------------------

      pos <- which(
        data_species[ , "common_en"] == var_selected() |
        data_species[ , "common_fr"] == var_selected() |
        data_species[ , "latin"]     == var_selected() |
        data_species[ , "inuktitut"] == var_selected()
      )

      var_english <- as.character(data_species[pos, "common_en"])


      titre <- paste0(
        var_english,
        " ",
        period(),
        ifelse(!is.null(rcp()), paste0(" [", rcp(), "]"), ""),
        ifelse(information() == "Observations", " (Observations)", ifelse(information() == "Uncertainties", " (Uncertainties)", ""))
      )


      if (period() == "1981-2010") {

        if (spclass() == "Aves") {

          datasource <- "BirdLife International & NatureServe"

        } else {

          datasource <- "IUCN"
        }

      } else {

        datasource <- NULL
      }


      ### MAP PRODUCTION -------------------------------------------------------

      mapQuebec(
        x          = ras(),
        title      = titre,
        type       = ifelse(suffix() == "species" & information() %in% c("Observations", "Binaries"), "binary", 1),
        datasource = datasource,
        palette    = gsub("-rev", "", couleur()),
        reverse    = ifelse(length(grep("-rev", couleur())) == 1, TRUE, FALSE),
        bins       = 7
      )


      ### DEVISE CLOSING -------------------------------------------------------

      dev.off()


      ### SUCCESS MESSAGE ------------------------------------------------------

      removeModal()

      showModal(
        modalDialog(

          title = "Congratulations!",

          HTML(
            paste0(
              "<div class=\"msg\">",
              "Your map has been downloaded.",
              "<br /><br />",
              "<i class=\"fas fa-check-circle fa-4x\"></i>",
              "<br /><br />",
              "</div>"
            )
          ),
          easyClose = TRUE,
          footer    = NULL
        )
      )
    }
  )


  ### CLICK ON POPUP DOWNLOAD BUTTON (CLIMATE) ---------------------------------

  output$download_climate <- downloadHandler(

    filename =  function() {


      ### EXPORTED MAP NAME ----------------------------------------------------

      paste0(
        "map-",
        gsub("[[:punct:]]|[[:space:]]", "", Sys.time()),
        ".",
        tolower(type())
      )
    },

    content  = function(file) {


      ### MESSAGE DURING MAP PREPARATION ---------------------------------------

      showModal(
        modalDialog(

          title = "Message",

          HTML(
            paste0(
              "<div class=\"msg\">",
              "Your map is being prepared.",
              "<br />",
              "Please wait a few seconds...",
              "<br /><br />",
              "<i class=\"fas fa-spinner faa-spin animated fa-4x\"></i>",
              "<br /><br />",
              "</div>"
            )
          ),
          easyClose = FALSE,
          footer    = NULL
        )
      )


      ### DEVISE INITIALISATION ------------------------------------------------

      if (type() == "PNG") {
        CairoPNG(
          filename  = file,
          width     = 5.00 * 1.2,
          height    = 5.80 * 1.2,
          pointsize = 14,
          bg        = "transparent",
          units     = "in",
          res       = dpi()
        )
      }

      if (type() == "JPEG") {
        CairoJPEG(
          filename  = file,
          width     = 5.00 * 1.2,
          height    = 5.80 * 1.2,
          pointsize = 14,
          bg        = "transparent",
          units     = "in",
          res       = dpi()
        )
      }

      if (type() == "TIFF") {
        CairoTIFF(
          filename  = file,
          width     = 5.00 * 1.2,
          height    = 5.80 * 1.2,
          pointsize = 14,
          bg        = "transparent",
          units     = "in",
          res       = dpi()
        )
      }

      if (type() == "PDF") {
        CairoPDF(
          file      = file,
          width     = 5.00 * 1.2,
          height    = 5.80 * 1.2,
          pointsize = 14,
          bg        = "transparent"
        )
      }


      ### MAP INFORMATIONS -----------------------------------------------------

      titre <- paste0(
        var_english(),
        ifelse(!is.null(var_units()), paste0(" (in ", var_units(), ") "), " "),
        period(),
        ifelse(!is.null(rcp()), paste0(" [", rcp(), "]"), ""),
        ifelse(information() == "Anomalies", " (Anomalies)", ifelse(information() == "Uncertainties", " (Uncertainties)", ""))
      )

      if (period() == "1981-2010") {

        datasource <- "NCEP-CFSR Reanalysis"

      } else {

        datasource <- "CORDEX/Ouranos"
      }


      ### MAP PRODUCTION -------------------------------------------------------

      mapQuebec(
        x          = ras(),
        title      = titre,
        type       = NULL,
        datasource = datasource,
        palette    = gsub("-rev", "", couleur()),
        reverse    = ifelse(length(grep("-rev", couleur())) == 1, TRUE, FALSE),
        bins       = 7
      )


      ### DEVISE CLOSING -------------------------------------------------------

      dev.off()


      ### SUCCESS MESSAGE ------------------------------------------------------

      removeModal()

      showModal(
        modalDialog(

          title = "Congratulations!",
          HTML(
            paste0(
              "<div class=\"msg\">",
              "Your map has been downloaded.",
              "<br /><br />",
              "<i class=\"fas fa-check-circle fa-4x\"></i>",
              "<br /><br />",
              "</div>"
            )
          ),
          easyClose = TRUE,
          footer    = NULL
        )
      )
    }
  )
}
