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

data_species   <- readRDS("data/infos/species_list.rds")
data_climate   <- readRDS("data/infos/variables_list.rds")
data_ecosystem <- readRDS("data/infos/ecosystem_list.rds")
data_network   <- readRDS("data/infos/network_list.rds")
grd            <- readRDS("data/background/grid.rds")
grd_tundra     <- readRDS("data/background/grid_tundra.rds")


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


  ###
  ### HIDE LIST OF COLOR PALETTES --------------------------------[[ ALL TABS ]]
  ###

  observe({

    for (i in c("climate", "species", "ecosystem")) {

      hide(id = paste0("color_", i))
    }
  })


  ###
  ### CATCH SELECTED TAB  ------------------------------------------------------
  ###

  suffix <- reactive({

    gsub(
      "tab_",
      "",
      reactive({
        input$nav
      })()
    )
  })


  ###
  ### DEFINE STUDY AREA  -------------------------------------------------------
  ###

  area <- reactive({

    if (suffix() %in% c("species", "climate")) {

      grd

    } else {

      grd_tundra
    }
  })


  ###
  ### DEFINE ZOOM LEVEL  -------------------------------------------------------
  ###

  coords <- reactive({

    if (suffix() %in% c("species", "climate")) {

      c(-45.50, 51.00, -75.50, 62.50)

    } else {

      c(-66.00, 55.65, -74.00, 63.25)
    }
  })


  ###
  ### FIRST DISPLAY OF LEAFLET MAPS (ON APP OPENNING) ------------[[ ALL TABS ]]
  ###

  for (i in c("map_species", "map_climate", "map_ecosystem")) {

    output[[i]] <- renderLeaflet({

      leaflet(
        data = area()
      ) %>%

      addProviderTiles(
        provider = providers$Esri.WorldTopoMap
      ) %>%

      addPolygons(
        color       = "#8e501d",
        weight      = 2,
        opacity     = 1.0,
        fillOpacity = 0,
        fillColor   = "transparent"
      ) %>%

      fitBounds(
        lng1 = ~coords()[1],
        lat1 = ~coords()[2],
        lng2 = ~coords()[3],
        lat2 = ~coords()[4]
      ) %>%

      addEasyButton(
        easyButton(
          icon    = "fa-globe",
          title   = "Zoom to study area",
          onClick = JS(
            paste0(
              "function(btn, ", i, "){ ",
              i, ".fitBounds(",
              "[[",
              coords()[2], ", ", coords()[3],
              "], [",
              coords()[4], ", ", coords()[1],
              "]]); }"
            )
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


  ### [[ CLIMATE TAB ]]--[[ CLIMATE TAB ]]--[[ CLIMATE TAB ]]--[[ CLIMATE TAB ]]
  ### [[ CLIMATE TAB ]]--[[ CLIMATE TAB ]]--[[ CLIMATE TAB ]]--[[ CLIMATE TAB ]]
  ### [[ CLIMATE TAB ]]--[[ CLIMATE TAB ]]--[[ CLIMATE TAB ]]--[[ CLIMATE TAB ]]


  observe({

    if (suffix() == "climate") {


      ###
      ### GET INPUTS VALUES AND SET VARIABLES -----------------[[ CLIMATE TAB ]]
      ###

      ### CATCH SELECTED LANGUAGE ----------------------------------------------

      langue <- reactive({

        input[["language_climate"]]
      })


      ### GET VARIABLES LIST FOR SELECTED LANGUAGE -----------------------------

      var_list <- reactive({

        c(
          "Select a variable" = "",
          sort(as.character(data_climate[ , langue()]))
        )
      })


      ### GET SELECTED VARIABLE (TRANSLATE IF REQUIRED) ------------------------

      var_selected <- reactive({

         # If no variable selected (RESET MAP)
         if (input[["select_climate"]] == "") {

          NULL

        } else {

           # If different variable in the same language (UPDATE MAP)
           if (length(which(var_list() == input[["select_climate"]])) == 1) {

            input[["select_climate"]]

          # If same variable in a different language (SAME MAP)
          } else {

            pos <- NULL

            for (i in c("english", "french")) {

              if (length(which(data_climate[ , i] == input[["select_climate"]])) > 0) {

                pos <- which(data_climate[ , i] == input[["select_climate"]])
              }
            }

            var_list()[which(var_list() == data_climate[pos, langue()])]
          }
        }
      })


      ### GET SELECTED TIME PERIOD ---------------------------------------------

      period <- reactive({

        input[["horizon_climate"]]
      })


      ### GET SELECTED RCP (ADAPT DEPENDING ON TIME PERIOD) --------------------

      rcp <- reactive({

        if (period() == "1981-2010") {

          NULL

        } else {

          if (is.null(input[["scenario_climate"]])) {

            "rcp45_climate"

          } else {

            input[["scenario_climate"]]
          }
        }
      })

      rcp_lab <- reactive({

        if (!is.null(rcp())) {

          if (rcp() == "rcp45_climate") {

            "RCP4.5"

          } else {

            "RCP8.5"
          }

        } else {

          NULL
        }
      })

      ### GET SELECTED VARIABLE ENGLISH NAME (FOR LEGEND) ----------------------

      var_english <- reactive({

        if (!is.null(var_selected())) {

          pos <- which(
            data_climate[ , "english"] == var_selected() |
            data_climate[ , "french"]  == var_selected()
          )

          as.character(data_climate[pos, "english"])

        } else {

          NULL
        }
      })


      ### GET VARIABLE UNITS (FOR LEGEND) --------------------------------------

      var_units <- reactive({

        if (!is.null(var_selected())) {

          pos <- which(
            data_climate[ , "english"] == var_selected() |
            data_climate[ , "french"]  == var_selected()
          )

          unit <- as.character(data_climate[pos, "units"])

          if (unit == "-") {

            NULL

          } else {

            as.character(data_climate[pos, "units"])

          }

        } else {

          NULL
        }
      })


      ### GET SELECTED INFORMATION (ADAPT DEPENDING ON TIME PERIOD) ------------

      information <- reactive({

        if (period() == "1981-2010"){

          "normals_climate"

        } else {

          input[["infos_climate"]]
        }
      })

      information_lab <- reactive({

        if (information() == "normals_climate") {

          "Climate normals"

        } else {

          if (information() == "uncertainties_climate") {

            "Uncertainties"

          } else {

            "Anomalies"
          }
        }
      })

      information_suf <- reactive({

        if (information() == "normals_climate") {

          "mean"

        } else {

          if (information() == "uncertainties_climate") {

            "sd"

          } else {

            "diff"
          }
        }
      })


      ### GET SELECTED RAMP COLOR ----------------------------------------------

      couleur <- reactive({

        input[["color_climate"]]
      })


      ### GET SELECTED DPI (FOR DOWNLOAD MAP) ----------------------------------

      dpi <- reactive({

        as.numeric(input[["dpi_climate"]])
      })


      ### GET SELECTED MAP OUTPUT TYPE (FOR DOWNLOAD MAP) ----------------------

      type <- reactive({

        input[["format_climate"]]
      })


      ### SET RASTER PATH ------------------------------------------------------

      raster_path <- reactive({

        if (!is.null(var_selected())) {

          pos <- which(
            data_climate[ , "english"] == var_selected() |
            data_climate[ , "french"]  == var_selected()
          )

          code <- data_climate[pos, "code"]

          if (period() == "1981-2010"){

            paste0(
              "data/climate/",
              code,
              "_",
              gsub("-", "", period()),
              "_",
              information_suf()
            )

          } else {

            paste0(
              "data/climate/",
              code,
              "_",
              gsub("-", "", period()),
              "_",
              tolower(gsub("\\.", "", rcp_lab())),
              "_",
              information_suf()
            )
          }

        } else {

          NULL
        }
      })


      ### IMPORT RASTER --------------------------------------------------------

      ras <- reactive({

        if (!is.null(var_selected())) {

          readRDS(paste0(raster_path(), ".rds"))

        } else {

          NULL
        }
      })


      ### CREATE COLOR PALETTE (BASED ON DATA) ---------------------------------

      couleurs <- reactive({

        if (!is.null(var_selected())) {

          # Color palette
          mycol <- brewer.pal(
            n    = rampcolors[which(rampcolors[ , "palette"] == gsub("-rev", "", couleur())), "maxcolors"],
            name = gsub("-rev", "", couleur())
          )

          # Reverse colors (if required)
          if (length(grep("-rev", couleur())) == 1) {

            mycol <- mycol[length(mycol):1]
          }

          # Categorize data
          colorNumeric(
            palette  = mycol,
            domain   = values(ras()),
            na.color = "transparent"
          )

        } else {

          NULL
        }
      })


      ###
      ### UPDATE UI INPUTS ------------------------------------[[ CLIMATE TAB ]]
      ###


      ### UPDATE LIST OF CLIMATE VARIABLES (DEPENDING ON LANGUAGE) -------------

      updateSelectInput(
        session  = session,
        inputId  = "select_climate",
        label    = NULL,
        choices  = var_list(),
        selected = ifelse(!is.null(var_selected()), var_selected(), "")
      )


      if (period() == "1981-2010") {

        ### DISABLE RCP RADIO --------------------------------------------------

        shinyjs::disable(selector = "[type=radio][value='rcp45_climate']")
        shinyjs::disable(selector = "[type=radio][value='rcp85_climate']")
        shinyjs::runjs('$("#scenario_climate .shiny-options-group").css({"color": "#aaa"});')
        shinyjs::runjs('$("#scenario_climate .shiny-options-group label").css({"cursor": "not-allowed"});')
        shinyjs::runjs('$("[type=radio][value=\'rcp45_climate\']").prop("checked", false);')
        shinyjs::runjs('$("[type=radio][value=\'rcp85_climate\']").prop("checked", false);')


        ### DISABLE ANOMALIES & UNCERTAINTIES ----------------------------------

        shinyjs::disable(selector = "[type=radio][value='anomalies_climate']")
        shinyjs::disable(selector = "[type=radio][value='uncertainties_climate']")
        shinyjs::runjs('$("#infos_climate .shiny-options-group .radio:nth-child(2)").css({"color": "#aaa"});')
        shinyjs::runjs('$("#infos_climate .shiny-options-group .radio:nth-child(2) label").css({"cursor": "not-allowed"});')
        shinyjs::runjs('$("#infos_climate .shiny-options-group .radio:nth-child(3)").css({"color": "#aaa"});')
        shinyjs::runjs('$("#infos_climate .shiny-options-group .radio:nth-child(3) label").css({"cursor": "not-allowed"});')
        shinyjs::runjs('$("[type=radio][value=\'anomalies_climate\']").prop("checked", false);')
        shinyjs::runjs('$("[type=radio][value=\'uncertainties_climate\']").prop("checked", false);')
        updateRadioButtons(session, inputId = "infos_climate", selected = information())

      } else {


        ### ENABLE RCP RADIO ---------------------------------------------------

        shinyjs::enable(selector = "[type=radio][value='rcp45_climate']")
        shinyjs::enable(selector = "[type=radio][value='rcp85_climate']")
        shinyjs::runjs('$("#scenario_climate .shiny-options-group").css("color", "#555");')
        shinyjs::runjs('$("#scenario_climate .shiny-options-group label").css({"cursor": "pointer"});')

        if (is.null(rcp())){

          updateRadioButtons(session, inputId = "scenario_climate", selected = "rcp45_climate")

        } else {

          updateRadioButtons(session, inputId = "scenario_climate", selected = rcp())

        }

        ### ENABLE ANOMALIES & UNCERTAINTIES -----------------------------------

        shinyjs::enable(selector = "[type=radio][value='anomalies_climate']")
        shinyjs::enable(selector = "[type=radio][value='uncertainties_climate']")
        shinyjs::runjs('$("#infos_climate .shiny-options-group .radio:nth-child(2)").css({"color": "#555"});')
        shinyjs::runjs('$("#infos_climate .shiny-options-group .radio:nth-child(2) label").css({"cursor": "pointer"});')
        shinyjs::runjs('$("#infos_climate .shiny-options-group .radio:nth-child(3)").css({"color": "#555"});')
        shinyjs::runjs('$("#infos_climate .shiny-options-group .radio:nth-child(3) label").css({"cursor": "pointer"});')
      }

      ### ENABLE / DISABLE DPI OPTION ------------------------------------------

      if (!is.null(type())) {
        if (type() == "PDF") {
          shinyjs::disable(id = "dpi_climate")
        } else {
          shinyjs::enable(id = "dpi_climate")
        }
      }


      ###
      ### SHOW / HIDE COLOR PALETTES --------------------------[[ CLIMATE TAB ]]
      ###

      onclick("grad_climate", function() {
        toggle(id = "menu_climate")
        toggleClass(id = "grad_climate", class = "shadow")
      })

      onclick("menu_climate", function(){
        toggle(id = "menu_climate")
        toggleClass(id = "grad_climate", class = "shadow")
      })


      ###
      ### CLICK ON HELP BUTTON --------------------------------[[ CLIMATE TAB ]]
      ###

      onclick("help-climate", function(){

        showModal(
          modalDialog(

            title = "Additional informations",

            includeHTML("includes/help-climate.html"),

            easyClose = TRUE,
            footer    = NULL
          )
        )
      })


      ###
      ### UPDATE/RESET MAP ------------------------------------[[ CLIMATE TAB ]]
      ###


      ### UPDATE MAP -----------------------------------------------------------

      if (!is.null(var_selected())) {

        leafletProxy(
          mapId = "map_climate",
          data  = area()
        ) %>%

        clearShapes() %>% clearControls() %>% clearImages() %>%

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
          lng1 = ~coords()[1],
          lat1 = ~coords()[2],
          lng2 = ~coords()[3],
          lat2 = ~coords()[4]
        ) %>%

        addLegend(
          position  = "bottomleft",
          pal       = couleurs(),
          values    = ras()[],
          title     = information_lab(),
          opacity   = 1,
          className = "info legend"
        )


      ### RESET MAP ------------------------------------------------------------

      } else {

        leafletProxy(
          mapId = "map_climate",
          data  = area()
        ) %>%
        clearShapes() %>% clearControls() %>% clearImages() %>%

        addPolygons(
          color       = "#8e501d",
          weight      = 2.0,
          opacity     = 1.0,
          fillOpacity = 0,
          fillColor   = "transparent"
        ) %>%

        fitBounds(
          lng1 = ~coords()[1],
          lat1 = ~coords()[2],
          lng2 = ~coords()[3],
          lat2 = ~coords()[4]
        )
      }


      ###
      ### CLICK ON DOWNLOAD BUTTON (MAIN PANEL) ---------------[[ CLIMATE TAB ]]
      ###

      onclick("btn-climate", function(){


        ### SHOW DOWNLOAD PARAMETERS -------------------------------------------

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


        ### ERROR MESSAGE (NO VARIABLE SELECTED) -------------------------------

        } else {

          showModal(
            modalDialog(

              title = "Warning!",

              HTML(
                paste0(
                  "<div class=\"msg\">",
                  "Please select a climate variable.",
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


      ###
      ### CLICK DOWNLOAD BUTTON (POPUP WINDOW) ----------------[[ CLIMATE TAB ]]
      ###

      output$download_climate <- downloadHandler(

        filename =  function() {


          ### EXPORTED MAP NAME ------------------------------------------------

          paste0(
            "map-",
            gsub("[[:punct:]]|[[:space:]]", "", Sys.time()),
            ".",
            tolower(type())
          )
        },

        content  = function(file) {


          ### MESSAGE DURING MAP PREPARATION -----------------------------------

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


          ### DEVISE INITIALISATION --------------------------------------------

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


          ### MAP INFORMATIONS -------------------------------------------------

          titre <- paste0(
            var_english(),
            ifelse(
              !is.null(var_units()),
              paste0(" (in ", var_units(), ") "),
              " "
            ),
            period(),
            ifelse(
              !is.null(rcp()),
              paste0(" [", rcp_lab(), "]"),
              ""
            ),
            ifelse(
              information_lab() == "Anomalies",
              " (Anomalies)",
              ifelse(
                information_lab() == "Uncertainties",
                " (Uncertainties)",
                ""
              )
            )
          )

          if (period() == "1981-2010") {

            datasource <- "NCEP-CFSR Reanalysis"

          } else {

            datasource <- "CORDEX/Ouranos"
          }


          ### MAP PRODUCTION ---------------------------------------------------

          mapQuebec(
            x          = ras(),
            title      = titre,
            type       = NULL,
            datasource = datasource,
            palette    = gsub("-rev", "", couleur()),
            reverse    = ifelse(length(grep("-rev", couleur())) == 1, TRUE, FALSE),
            bins       = 7
          )


          ### DEVISE CLOSING ---------------------------------------------------

          dev.off()


          ### SUCCESS MESSAGE --------------------------------------------------

          removeModal()

          showModal(
            modalDialog(

              title = "Congratulations!",
              HTML(
                paste0(
                  "<div class=\"msg\">",
                  "Your map has been successfully downloaded.",
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
  })


  ### [[ SPECIES TAB ]]--[[ SPECIES TAB ]]--[[ SPECIES TAB ]]--[[ SPECIES TAB ]]
  ### [[ SPECIES TAB ]]--[[ SPECIES TAB ]]--[[ SPECIES TAB ]]--[[ SPECIES TAB ]]
  ### [[ SPECIES TAB ]]--[[ SPECIES TAB ]]--[[ SPECIES TAB ]]--[[ SPECIES TAB ]]

  observe({

    if (suffix() == "species") {


      ###
      ### GET INPUTS VALUES AND SET VARIABLES -----------------[[ SPECIES TAB ]]
      ###

      ### CATCH SELECTED LANGUAGE ----------------------------------------------

      langue <- reactive({

        input[["language_species"]]
      })


      ### CATCH SELECTED SPECIES GROUP -----------------------------------------

      spclass <- reactive({

        input[["class_species"]]
      })


      ### GET VARIABLES LIST FOR SELECTED LANGUAGE -----------------------------

      var_list <- reactive({

        c(
          "Select a species"  = "",
          sort(as.character(data_species[which(data_species[ , "class"] == spclass()), langue()]))
        )
      })


      ### GET SELECTED VARIABLE (TRANSLATE IF REQUIRED) ------------------------

      var_selected <- reactive({

         # If no species selected (RESET MAP)
         if (input[["select_species"]] == "") {

          NULL

        } else {

           # If different species in the same language (UPDATE MAP)
           if (length(which(var_list() == input[["select_species"]])) == 1) {

            input[["select_species"]]

          } else {

            # If same language but different class (RESET MAP)
            if (length(which(data_species[ , langue()] == input[["select_species"]])) == 1) {

              NULL

            # If same species in a different language
            } else {

              pos <- NULL

              for (i in c("common_en", "common_fr", "latin", "inuktitut")) {

                if (length(which(data_species[ , i] == input[["select_species"]])) > 0) {

                  pos <- which(data_species[ , i] == input[["select_species"]])
                }
              }

              pos <- which(var_list() == data_species[pos, langue()])

              # Match found (SAME MAP)
              if (length(pos) == 1) {

                var_list()[pos]

              # No correspondance found in Inuktituk (RESET MAP)
              } else {

                NULL
              }
            }
          }
        }
      })


      ### GET SELECTED TIME PERIOD ---------------------------------------------

      period <- reactive({

        input[["horizon_species"]]
      })


      ### GET SELECTED RCP (ADAPT DEPENDING ON TIME PERIOD) --------------------

      rcp <- reactive({

        if (period() == "1981-2010") {

          NULL

        } else {

          if (is.null(input[["scenario_species"]])) {

            "rcp45_species"

          } else {

            input[["scenario_species"]]
          }
        }
      })

      rcp_lab <- reactive({

        if (!is.null(rcp())) {

          if (rcp() == "rcp45_species") {

            "RCP4.5"

          } else {

            "RCP8.5"
          }

        } else {

          NULL
        }
      })


      ### GET SELECTED VARIABLE ENGLISH NAME (FOR LEGEND) ----------------------

      var_english <- reactive({

        if (!is.null(var_selected())) {

          pos <- which(
            data_species[ , "common_en"] == var_selected() |
            data_species[ , "common_fr"] == var_selected() |
            data_species[ , "latin"]     == var_selected() |
            data_species[ , "inuktitut"] == var_selected()
          )

          as.character(data_species[pos, "common_en"])

        } else {

          NULL
        }
      })


      ### GET SELECTED INFORMATION (ADAPT DEPENDING ON TIME PERIOD) ------------

      information <- reactive({

        if (period() != "1981-2010" && input[["infos_species"]] == "observations_species"){

          "binaries_species"

        } else {

          input[["infos_species"]]
        }
      })

      information_lab <- reactive({

        if (information() == "observations_species") {

          "Observations"

        } else {

          if (information() == "binaries_species") {

            "Binaries"

          } else {

            if (information() == "probabilities_species") {

              "Probabilities"

            } else {

              "Uncertainties"
            }
          }
        }
      })



      information_suf <- reactive({

        if (information() == "observations_species") {

          "observations"

        } else {

          if (information() == "binaries_species") {

            "binaries"

          } else {

            if (information() == "probabilities_species") {

              "probabilities"

            } else {

              "uncertainties"
            }
          }
        }
      })


      ### GET SELECTED RAMP COLOR ----------------------------------------------

      couleur <- reactive({

        input[["color_species"]]
      })


      ### GET SELECTED DPI (FOR DOWNLOAD MAP) ----------------------------------

      dpi <- reactive({

        as.numeric(input[["dpi_species"]])
      })


      ### GET SELECTED MAP OUTPUT TYPE (FOR DOWNLOAD MAP) ----------------------

      type <- reactive({

        input[["format_species"]]
      })


      ### SET RASTER PATH ------------------------------------------------------

      raster_path <- reactive({

        if (!is.null(var_selected())) {

          pos <- which(
            data_species[ , "common_en"] == var_selected() |
            data_species[ , "common_fr"] == var_selected() |
            data_species[ , "latin"]     == var_selected() |
            data_species[ , "inuktitut"] == var_selected()
          )

          code <- data_species[pos, "code"]

          if (period() == "1981-2010"){

            paste0(
              "data/",
              tolower(spclass()),
              "/",
              code,
              "_",
              period(),
              "_",
              information_suf()
            )

          } else {

            paste0(
              "data/",
              tolower(spclass()),
              "/",
              code,
              "_",
              period(),
              "_",
              tolower(gsub("\\.", "", rcp_lab())),
              "_",
              information_suf()
            )
          }

        } else {

          NULL
        }
      })


      ### IMPORT RASTER --------------------------------------------------------

      ras <- reactive({

        if (!is.null(var_selected())) {

          ras <- readRDS(paste0(raster_path(), ".rds"))

          # Rescale probabilities
          if (information_lab() == "Probabilities") {

            ras[][which(!is.na(ras[]))] <- ifelse(
              ras[][which(!is.na(ras[]))] > 1,
              1,
              ras[][which(!is.na(ras[]))]
            )
          }

          ras

        } else {

          NULL
        }
      })


      ### CREATE COLOR PALETTE (BASED ON DATA) ---------------------------------

      couleurs <- reactive({

        if (!is.null(var_selected())) {

          # Color palette
          mycol <- brewer.pal(
            n    = rampcolors[which(rampcolors[ , "palette"] == gsub("-rev", "", couleur())), "maxcolors"],
            name = gsub("-rev", "", couleur())
          )

          # Reverse colors (if required)
          if (length(grep("-rev", couleur())) == 1) {

            mycol <- mycol[length(mycol):1]
          }

          # More than 2 values
          if (length(unique(values(ras()))) > 2) {

            colorNumeric(
              palette  = mycol,
              domain   = values(ras()),
              na.color = "transparent"
            )

          } else {

            # If one single value (only presence or only absence)
            if (length(which(unique(values(ras())) == 0) == 1)) {

              colorNumeric(
                palette  = mycol[1],
                domain   = values(ras()),
                na.color = "transparent"
              )

            # If binary data
            } else {

              colorNumeric(
                palette  = mycol[length(mycol)],
                domain   = values(ras()),
                na.color = "transparent"
              )
            }
          }

        } else {

          NULL
        }
      })


      ###
      ### UPDATE UI INPUTS ------------------------------------[[ SPECIES TAB ]]
      ###

      ### UPDATE LIST OF CLIMATE VARIABLES (DEPENDING ON LANGUAGE) -------------

      updateSelectInput(
        session  = session,
        inputId  = "select_species",
        label    = NULL,
        choices  = var_list(),
        selected = ifelse(!is.null(var_selected()), var_selected(), "")
      )


      if (period() == "1981-2010") {

        ### DISABLE RCP RADIO --------------------------------------------------

        shinyjs::disable(selector = "[type=radio][value='rcp45_species']")
        shinyjs::disable(selector = "[type=radio][value='rcp85_species']")
        shinyjs::runjs('$("#scenario_species .shiny-options-group").css({"color": "#aaa"});')
        shinyjs::runjs('$("#scenario_species .shiny-options-group label").css({"cursor": "not-allowed"});')
        shinyjs::runjs('$("[type=radio][value=\'rcp45_species\']").prop("checked", false);')
        shinyjs::runjs('$("[type=radio][value=\'rcp85_species\']").prop("checked", false);')


        ### ENABLE OBSERVATIONS ------------------------------------------------

        shinyjs::enable(selector = "[type=radio][value='observations_species']")
        shinyjs::runjs('$("#infos_species .shiny-options-group .radio:first-child").css({"color": "#555"});')
        shinyjs::runjs('$("#infos_species .shiny-options-group .radio:first-child label").css({"cursor": "pointer"});')

      } else {


        ### ENABLE RCP RADIO ---------------------------------------------------

        shinyjs::enable(selector = "[type=radio][value='rcp45_species']")
        shinyjs::enable(selector = "[type=radio][value='rcp85_species']")
        shinyjs::runjs('$("#scenario_species .shiny-options-group").css("color", "#555");')
        shinyjs::runjs('$("#scenario_species .shiny-options-group label").css({"cursor": "pointer"});')

        if (is.null(rcp())){

          updateRadioButtons(session, inputId = "scenario_species", selected = "rcp45_species")

        } else {

          updateRadioButtons(session, inputId = "scenario_species", selected = rcp())

        }

        ### DISABLE OBSERVATIONS -----------------------------------------------

        shinyjs::disable(selector = "[type=radio][value='observations_species']")
        shinyjs::runjs('$("#infos_species .shiny-options-group .radio:first-child").css({"color": "#aaa"});')
        shinyjs::runjs('$("#infos_species .shiny-options-group .radio:first-child label").css({"cursor": "not-allowed"});')
        updateRadioButtons(session, inputId = "infos_species", selected = information())
      }

      ### ENABLE / DISABLE DPI OPTION ------------------------------------------

      if (!is.null(type())) {
        if (type() == "PDF") {
          shinyjs::disable(id = "dpi_species")
        } else {
          shinyjs::enable(id = "dpi_species")
        }
      }


      ###
      ### SHOW / HIDE COLOR PALETTES --------------------------[[ SPECIES TAB ]]
      ###

      onclick("grad_species", function() {
        toggle(id = "menu_species")
        toggleClass(id = "grad_species", class = "shadow")
      })

      onclick("menu_species", function(){
        toggle(id = "menu_species")
        toggleClass(id = "grad_species", class = "shadow")
      })


      ###
      ### CLICK ON HELP BUTTON --------------------------------[[ SPECIES TAB ]]
      ###

      onclick("help-species", function(){

        showModal(
          modalDialog(

            title = "Additional informations",

            includeHTML("includes/help-species.html"),

            easyClose = TRUE,
            footer    = NULL
          )
        )
      })


      ###
      ### UPDATE/RESET MAP ------------------------------------[[ SPECIES TAB ]]
      ###


      ### UPDATE MAP -----------------------------------------------------------

      if (!is.null(var_selected())) {

        leafletProxy(
          mapId = "map_species",
          data  = area()
        ) %>%

        clearShapes() %>% clearControls() %>% clearImages() %>%

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
          lng1 = ~coords()[1],
          lat1 = ~coords()[2],
          lng2 = ~coords()[3],
          lat2 = ~coords()[4]
        ) %>%

        addLegend(
          position  = "bottomleft",
          pal       = couleurs(),
          values    = ras()[],
          title     = information_lab(),
          opacity   = 1,
          className = "info legend"
        )


      ### RESET MAP ------------------------------------------------------------

      } else {

        leafletProxy(
          mapId = "map_species",
          data  = area()
        ) %>%
        clearShapes() %>% clearControls() %>% clearImages() %>%

        addPolygons(
          color       = "#8e501d",
          weight      = 2.0,
          opacity     = 1.0,
          fillOpacity = 0,
          fillColor   = "transparent"
        ) %>%

        fitBounds(
          lng1 = ~coords()[1],
          lat1 = ~coords()[2],
          lng2 = ~coords()[3],
          lat2 = ~coords()[4]
        )
      }


      ###
      ### CLICK ON DOWNLOAD BUTTON (MAIN PANEL) ---------------[[ SPECIES TAB ]]
      ###

      onclick("btn-species", function(){


        ### SHOW DOWNLOAD PARAMETERS -------------------------------------------

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


        ### ERROR MESSAGE (NO VARIABLE SELECTED) -------------------------------

        } else {

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


      ###
      ### CLICK DOWNLOAD BUTTON (POPUP WINDOW) ----------------[[ SPECIES TAB ]]
      ###

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

          # var_english <- as.character(data_species[pos, "common_en"])


          titre <- paste0(
            var_english(),
            " ",
            period(),
            ifelse(
              !is.null(rcp()),
              paste0(" [", rcp_lab(), "]"),
              ""
            ),
            ifelse(
              information_lab() == "Observations",
              " (Observations)",
              ifelse(
                information_lab() == "Uncertainties",
                " (Uncertainties)",
                ""
              )
            )
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
            type       = ifelse(information_lab() %in% c("Observations", "Binaries"), "binary", 1),
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
  })



  ###  [[ ECOSYSTEM TAB ]]--------[[ ECOSYSTEM TAB ]]--------[[ ECOSYSTEM TAB ]]
  ###  [[ ECOSYSTEM TAB ]]--------[[ ECOSYSTEM TAB ]]--------[[ ECOSYSTEM TAB ]]
  ###  [[ ECOSYSTEM TAB ]]--------[[ ECOSYSTEM TAB ]]--------[[ ECOSYSTEM TAB ]]

  observe({

    if (suffix() == "ecosystem") {


      ###
      ### GET INPUTS VALUES AND SET VARIABLES ---------------[[ ECOSYSTEM TAB ]]
      ###

      ### CATCH SELECTED SPECIES GROUP -----------------------------------------

      spclass <- reactive({

        input[["class_ecosystem"]]
      })


      ### GET SELECTED TIME PERIOD ---------------------------------------------

      period <- reactive({

        input[["horizon_ecosystem"]]
      })


      ### GET SELECTED RCP (ADAPT DEPENDING ON TIME PERIOD) --------------------

      rcp <- reactive({

        if (period() == "1981-2010") {

          NULL

        } else {

          if (is.null(input[["scenario_ecosystem"]])) {

            "rcp45_ecosystem"

          } else {

            input[["scenario_ecosystem"]]
          }
        }
      })

      rcp_lab <- reactive({

        if (!is.null(rcp())) {

          if (rcp() == "rcp45_ecosystem") {

            "RCP4.5"

          } else {

            "RCP8.5"
          }

        } else {

          NULL
        }
      })


      ### GET SELECTED INFORMATION (ADAPT DEPENDING ON TIME PERIOD) ------------

      information <- reactive({

        if (period() == "1981-2010"){

          "richness"

        } else {

          input[["infos_ecosystem"]]
        }
      })

      information_lab <- reactive({

        if (information() == "richness") {

          if (spclass() == "total") {

            "Number of species"

          } else {

            if (spclass() == "birds") {

              "Number of species<br />(birds)"

            } else {

              "Number of species<br />(mammals)"
            }
          }

        } else {

          if (information() == "gains") {

            if (spclass() == "total") {

              "Species gains (%)"

            } else {

              if (spclass() == "birds") {

                "Species gains (%)<br />(birds)"

              } else {

                "Species gains (%)<br />(mammals)"
              }
            }

          } else {

            if (information() == "losses") {

              if (spclass() == "total") {

                "Species losses (%)"

              } else {

                if (spclass() == "birds") {

                  "Species losses (%)<br />(birds)"

                } else {

                  "Species losses (%)<br />(birds)"
                }
              }

            } else {

              if (spclass() == "total") {

                "Total turnover"

              } else {

                if (spclass() == "birds") {

                  "Birds turnover"

                } else {

                  "Mammals turnover"
                }
              }
            }
          }
        }
      })

      information_lab2 <- reactive({

        if (information() == "richness") {

          if (spclass() == "total") {

            "Total number of species"

          } else {

            if (spclass() == "birds") {

              "Number of birds species"

            } else {

              "Number of mammals species"
            }
          }

        } else {

          if (information() == "gains") {

            if (spclass() == "total") {

              "Percentage of species gains"

            } else {

              if (spclass() == "birds") {

                "Percentage of birds species gains"

              } else {

                "Percentage of mammals species gains"
              }
            }

          } else {

            if (information() == "losses") {

              if (spclass() == "total") {

                "Percentage of species losses"

              } else {

                if (spclass() == "birds") {

                  "Percentage of birds species losses"

                } else {

                  "Percentage of mammals species losses"
                }
              }

            } else {

              if (spclass() == "total") {

                "Total turnover"

              } else {

                if (spclass() == "birds") {

                  "Birds turnover"

                } else {

                  "Mammals turnover"
                }
              }
            }
          }
        }
      })


      ### GET SELECTED RAMP COLOR ----------------------------------------------

      couleur <- reactive({

        input[["color_ecosystem"]]
      })


      ### GET SELECTED DPI (FOR DOWNLOAD MAP) ----------------------------------

      dpi <- reactive({

        as.numeric(input[["dpi_ecosystem"]])
      })


      ### GET SELECTED MAP OUTPUT TYPE (FOR DOWNLOAD MAP) ----------------------

      type <- reactive({

        input[["format_ecosystem"]]
      })


      ### SET RASTER PATH ------------------------------------------------------

      raster_path <- reactive({

        pos <- which(
          data_ecosystem[ , "info"]  == information() &
          data_ecosystem[ , "class"] == spclass()
        )

        code <- data_ecosystem[pos, "code"]

        if (period() == "1981-2010"){

          paste0(
            "data/ecosystem/",
            code,
            "_",
            gsub("-", "", period())
          )

        } else {

          paste0(
            "data/ecosystem/",
            code,
            "_",
            gsub("-", "", period()),
            "_",
            tolower(gsub("\\.", "", rcp_lab()))
          )
        }
      })

      ### IMPORT RASTER --------------------------------------------------------

      ras <- reactive({

        readRDS(paste0(raster_path(), ".rds"))
      })


      ### CREATE COLOR PALETTE (BASED ON DATA) ---------------------------------

      couleurs <- reactive({

        # Color palette
        mycol <- brewer.pal(
          n    = rampcolors[which(rampcolors[ , "palette"] == gsub("-rev", "", couleur())), "maxcolors"],
          name = gsub("-rev", "", couleur())
        )

        # Reverse colors (if required)
        if (length(grep("-rev", couleur())) == 1) {

          mycol <- mycol[length(mycol):1]
        }

        # Categorize data
        colorNumeric(
          palette  = mycol,
          domain   = values(ras()),
          na.color = "transparent"
        )
      })


      ###
      ### UPDATE UI INPUTS ----------------------------------[[ ECOSYSTEM TAB ]]
      ###


      if (period() == "1981-2010") {

        ### DISABLE RCP RADIO --------------------------------------------------

        shinyjs::disable(selector = "[type=radio][value='rcp45_ecosystem']")
        shinyjs::disable(selector = "[type=radio][value='rcp85_ecosystem']")
        shinyjs::runjs('$("#scenario_ecosystem .shiny-options-group").css({"color": "#aaa"});')
        shinyjs::runjs('$("#scenario_ecosystem .shiny-options-group label").css({"cursor": "not-allowed"});')
        shinyjs::runjs('$("[type=radio][value=\'rcp45_ecosystem\']").prop("checked", false);')
        shinyjs::runjs('$("[type=radio][value=\'rcp85_ecosystem\']").prop("checked", false);')


        ### DISABLE BUTTONS ----------------------------------------------------

        shinyjs::disable(selector = "[type=radio][value='losses']")
        shinyjs::runjs('$("#infos_ecosystem .shiny-options-group .radio:nth-child(2)").css({"color": "#aaa"});')
        shinyjs::runjs('$("#infos_ecosystem .shiny-options-group .radio:nth-child(2) label").css({"cursor": "not-allowed"});')
        shinyjs::disable(selector = "[type=radio][value='gains']")
        shinyjs::runjs('$("#infos_ecosystem .shiny-options-group .radio:nth-child(3)").css({"color": "#aaa"});')
        shinyjs::runjs('$("#infos_ecosystem .shiny-options-group .radio:nth-child(3) label").css({"cursor": "not-allowed"});')
        shinyjs::disable(selector = "[type=radio][value='turnover']")
        shinyjs::runjs('$("#infos_ecosystem .shiny-options-group .radio:nth-child(4)").css({"color": "#aaa"});')
        shinyjs::runjs('$("#infos_ecosystem .shiny-options-group .radio:nth-child(4) label").css({"cursor": "not-allowed"});')

        updateRadioButtons(session, inputId = "infos_ecosystem", selected = "richness")

      } else {


        ### ENABLE RCP RADIO ---------------------------------------------------

        shinyjs::enable(selector = "[type=radio][value='rcp45_ecosystem']")
        shinyjs::enable(selector = "[type=radio][value='rcp85_ecosystem']")
        shinyjs::runjs('$("#scenario_ecosystem .shiny-options-group").css("color", "#555");')
        shinyjs::runjs('$("#scenario_ecosystem .shiny-options-group label").css({"cursor": "pointer"});')

        if (is.null(rcp())){

          updateRadioButtons(session, inputId = "scenario_ecosystem", selected = "rcp45_ecosystem")

        } else {

          updateRadioButtons(session, inputId = "scenario_ecosystem", selected = rcp())

        }

        ### ENABLE BUTTONS -----------------------------------------------------

        shinyjs::enable(selector = "[type=radio][value='losses']")
        shinyjs::runjs('$("#infos_ecosystem .shiny-options-group .radio:nth-child(2)").css({"color": "#555"});')
        shinyjs::runjs('$("#infos_ecosystem .shiny-options-group .radio:nth-child(2) label").css({"cursor": "pointer"});')
        shinyjs::enable(selector = "[type=radio][value='gains']")
        shinyjs::runjs('$("#infos_ecosystem .shiny-options-group .radio:nth-child(3)").css({"color": "#555"});')
        shinyjs::runjs('$("#infos_ecosystem .shiny-options-group .radio:nth-child(3) label").css({"cursor": "pointer"});')
        shinyjs::enable(selector = "[type=radio][value='turnover']")
        shinyjs::runjs('$("#infos_ecosystem .shiny-options-group .radio:nth-child(4)").css({"color": "#555"});')
        shinyjs::runjs('$("#infos_ecosystem .shiny-options-group .radio:nth-child(4) label").css({"cursor": "pointer"});')

        updateRadioButtons(session, inputId = "infos_ecosystem", selected = information())

      }

      ### ENABLE / DISABLE DPI OPTION ------------------------------------------

      if (!is.null(type())) {
        if (type() == "PDF") {
          shinyjs::disable(id = "dpi_ecosystem")
        } else {
          shinyjs::enable(id = "dpi_ecosystem")
        }
      }


      ###
      ### SHOW / HIDE COLOR PALETTES ------------------------[[ ECOSYSTEM TAB ]]
      ###

      onclick("grad_ecosystem", function() {
        toggle(id = "menu_ecosystem")
        toggleClass(id = "grad_ecosystem", class = "shadow")
      })

      onclick("menu_ecosystem", function(){
        toggle(id = "menu_ecosystem")
        toggleClass(id = "grad_ecosystem", class = "shadow")
      })


      ###
      ### CLICK ON HELP BUTTON ------------------------------[[ ECOSYSTEM TAB ]]
      ###

      onclick("help-ecosystem", function(){

        showModal(
          modalDialog(

            title = "Additional informations",

            includeHTML("includes/help-ecosystem.html"),

            easyClose = TRUE,
            footer    = NULL
          )
        )
      })


      ###
      ### UPDATE MAP ----------------------------------------[[ ECOSYSTEM TAB ]]
      ###

      leafletProxy(
        mapId = "map_ecosystem",
        data  = area()
      ) %>%

      clearShapes() %>% clearControls() %>% clearImages() %>%

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
        lng1 = ~coords()[1],
        lat1 = ~coords()[2],
        lng2 = ~coords()[3],
        lat2 = ~coords()[4]
      ) %>%

      addLegend(
        position  = "bottomleft",
        pal       = couleurs(),
        values    = ras()[],
        title     = information_lab(),
        opacity   = 1,
        className = "info legend"
      )


      ###
      ### CLICK ON DOWNLOAD BUTTON (MAIN PANEL) -------------[[ ECOSYSTEM TAB ]]
      ###

      onclick("btn-ecosystem", function(){


        ### SHOW DOWNLOAD PARAMETERS -------------------------------------------

        showModal(
          modalDialog(

            title = "Download options",

            HTML("<div id=\"save_ecosystem\">"),

            fluidRow(

              column(6,
                radioButtons(
                  inputId  = "format_ecosystem",
                  label    = "Output format:",
                  choices  = c("PNG", "JPEG", "TIFF", "PDF"),
                  selected = "PNG",
                  inline   = FALSE
                )
              ),

              column(6,
                selectInput(
                  inputId  = "dpi_ecosystem",
                  label    = "Resolution (dpi):",
                  choices  = c(72, 96, 150, 300, 600, 900),
                  selected = 300,
                  width    = 150
                )
              )
            ),

            HTML("<div class=\"bouton\">"),
            downloadButton("download_ecosystem", "Download map"),
            HTML("<br /></div></div>"),

            easyClose = TRUE,
            footer    = NULL
          )
        )
      })


      ###
      ### CLICK DOWNLOAD BUTTON (POPUP WINDOW) --------------[[ ECOSYSTEM TAB ]]
      ###

      output$download_ecosystem <- downloadHandler(

        filename =  function() {


          ### EXPORTED MAP NAME ------------------------------------------------

          paste0(
            "map-",
            gsub("[[:punct:]]|[[:space:]]", "", Sys.time()),
            ".",
            tolower(type())
          )
        },

        content  = function(file) {


          ### MESSAGE DURING MAP PREPARATION -----------------------------------

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


          ### DEVISE INITIALISATION --------------------------------------------

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


          ### MAP INFORMATIONS -------------------------------------------------

          titre <- paste0(
            information_lab2(),
            " ",
            period(),
            ifelse(
              !is.null(rcp()),
              paste0(" [", rcp_lab(), "]"),
              ""
            )
          )

          datasource <- NULL


          ### MAP PRODUCTION ---------------------------------------------------

          mapTundra(
            x          = ras(),
            title      = titre,
            type       = NULL,
            datasource = datasource,
            palette    = gsub("-rev", "", couleur()),
            reverse    = ifelse(length(grep("-rev", couleur())) == 1, TRUE, FALSE),
            bins       = 7
          )


          ### DEVISE CLOSING ---------------------------------------------------

          dev.off()


          ### SUCCESS MESSAGE --------------------------------------------------

          removeModal()

          showModal(
            modalDialog(

              title = "Congratulations!",
              HTML(
                paste0(
                  "<div class=\"msg\">",
                  "Your map has been successfully downloaded.",
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
  })
}
