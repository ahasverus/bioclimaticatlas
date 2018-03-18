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


### CAIRO FONTS ----------------------------------------------------------------

cairoFont <- "Times New Roman"
CairoFonts(
  regular    = paste0(cairoFont, ":style=Regular"),
  bold       = paste0(cairoFont, ":style=Bold"),
  italic     = paste0(cairoFont, ":style=Italic"),
  bolditalic = paste0(cairoFont, ":style=Bold Italic,BoldItalic")
)



server <- function(input, output, session) {



  ### HIDE LIST OF COLOR PALETTES ----------------------------------------------

  observe({

    for (i in c("climate", "species", "ecosystem", "network", "vulnerability")) {

      hide(id = paste0("color_", i))
    }
  })


  ### CATCH SELECTED TAB  ------------------------------------------------------

  suffix <- reactive({

    gsub(
      "tab_",
      "",
      reactive({
        input$nav
      })()
    )
  })


  ### SET APPROPRIATED DATASET  ------------------------------------------------

  data <- reactive({

    if (suffix() == "climate") {

      data_climate

    } else {

      if (suffix() == "species") {

        data_species

      } else {

        if (suffix() == "ecosystem") {

          data_ecosystem

        } else {

          if (suffix() == "network") {

            data_network

          } else {

            if (suffix() == "vulnerability") {

              data_vulnerability

            } else {

              NULL
            }
          }
        }
      }
    }
  })



  ### DEFINE LEAFLET STUDY AREA  -----------------------------------------------

  area <- reactive({

    if (suffix() %in% c("species", "climate")) {

      grd

    } else {

      grd_tundra
    }
  })


  ### DEFINE LEAFLET ZOOM LEVEL  -----------------------------------------------

  coords <- reactive({

    if (suffix() %in% c("species", "climate")) {

      c(-45.50, 51.00, -75.50, 62.50)

    } else {

      c(-66.00, 55.65, -74.00, 63.25)
    }
  })


  ### FIRST DISPLAY OF LEAFLET MAPS (ON APP OPENNING) --------------------------

  for (map in c("map_species", "map_climate", "map_ecosystem", "map_network", "map_vulnerability")) {

    output[[map]] <- renderLeaflet({

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
              "function(btn, ", map, "){ ",
              map, ".fitBounds(",
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


  ### CATCH SELECTED LANGUAGE --------------------------------------------------

  langue <- reactive({

    if (suffix() %in% c("climate", "species", "network")) {

      input[[paste0("language_", suffix())]]

    } else {

      NULL
    }
  })


  ### GET VARIABLE NAMES COLUMNS -----------------------------------------------

  columns <- reactive({

    if (suffix() == "species") {

      c("common_en", "common_fr", "latin", "inuktitut")

    } else {

      if (suffix() %in% c("climate", "network")) {

        c("english", "french")

      } else {

        "english"
      }
    }
  })


  ### CATCH SELECTED SPECIES GROUP ---------------------------------------------

  spclass <- reactive({

    if (suffix() %in% c("species", "ecosystem")) {

      input[[paste0("class_", suffix())]]

    } else {

      NULL
    }
  })


  ### GET VARIABLES LIST FOR THE SELECTED LANGUAGE -----------------------------

  var_list <- reactive({

    if (suffix() %in% c("climate", "species", "network")) {

      if (suffix() %in% c("climate", "network")) {

        rows  <- 1:nrow(data())
        empty <- c("Select a variable"  = "")

      } else {

        rows  <- which(data()[ , "class"] == spclass())
        empty <- c("Select a species"  = "")

      }

      c(empty, sort(as.character(data()[rows, langue()])))

    } else {

      NULL
    }
  })


  ### GET SELECTED VARIABLE (translate if required) ----------------------------

  var_selected <- reactive({

    if (suffix() %in% c("climate", "species", "network")) {

      # If no species/variable selected (RESET MAP)
      if (input[[paste0("select_", suffix())]] == "") {

        NULL

      } else {

        # If different species/variable in the same language (UPDATE MAP)
        if (length(which(var_list() == input[[paste0("select_", suffix())]])) == 1) {

          input[[paste0("select_", suffix())]]

        } else {

          # If same language but different class (RESET MAP) [only for species tab]
          if (length(which(data()[ , langue()] == input[[paste0("select_", suffix())]])) == 1) {

            NULL

          # If same species/variable in a different language
          } else {

            pos <- NULL

            for (i in columns()) {

              if (length(which(data()[ , i] == input[[paste0("select_", suffix())]])) > 0) {

                pos <- which(data()[ , i] == input[[paste0("select_", suffix())]])
              }
            }

            pos <- which(var_list() == data()[pos, langue()])

            # If match found (SAME MAP)
            if (length(pos) == 1) {

              var_list()[pos]

            # If no correspondance found (RESET MAP) [Inuktituk]
            } else {

              NULL
            }
          }
        }
      }
    } else {

      if (suffix() == "ecosystem") {

        "radio"

      } else {

        if (suffix() == "vulnerability") {

          "Vulnerability index"

        } else {

          NULL
        }
      }
    }
  })


  ### GET VARIABLE UNITS (for exported map legend) -----------------------------

  var_units <- reactive({

    if (!is.null(var_selected())) {

      if (suffix() == "climate") {

        pos <- NULL

        for (i in columns()) {

          if (length(which(data()[ , i] == var_selected())) == 1) {

            pos <- which(data()[ , i] == var_selected())
          }
        }

        unit <- as.character(data()[pos, "units"])

        if (unit == "-") {

          NULL

        } else {

          as.character(data()[pos, "units"])

        }
      } else {

        NULL
      }
    } else {

      NULL
    }
  })


  ### GET SELECTED TIME PERIOD -------------------------------------------------

  period <- reactive({

    if (suffix() %in% c("climate", "species", "ecosystem", "network", "vulnerability")) {

      input[[paste0("horizon_", suffix())]]

    } else {

      NULL
    }
  })


  ### GET SELECTED RCP (adapt depending on time period) ------------------------

  rcp <- reactive({

    if (suffix() %in% c("climate", "species", "ecosystem", "network", "vulnerability")) {

      if (period() == "1981-2010") {

        NULL

      } else {

        if (is.null(input[[paste0("scenario_", suffix())]])) {

          paste0("rcp45_", suffix())

        } else {

          input[[paste0("scenario_", suffix())]]

        }
      }

    } else {

      NULL
    }
  })


  ### SET RCP LABEL (for exported map legend) ----------------------------------

  rcp_legend <- reactive({

    if (!is.null(rcp())) {

      if (rcp() == paste0("rcp45_", suffix())) {

        "RCP4.5"

      } else {

        "RCP8.5"
      }

    } else {

      NULL
    }
  })


  ### GET SELECTED INFORMATION (adapt depending on time period) ----------------

  information <- reactive({

    if (suffix() %in% c("climate", "species", "ecosystem", "network", "vulnerability")) {

      if (suffix() == "species") {

        if (period() != "1981-2010" && input[[paste0("infos_", suffix())]] == paste0("observations_", suffix())){

          paste0("binaries_", suffix())

        } else {

          input[[paste0("infos_", suffix())]]
        }

      } else {

        if (suffix() == "climate") {

          if (period() == "1981-2010"){

            paste0("normals_", suffix())

          } else {

            input[[paste0("infos_", suffix())]]

          }

        } else {

          if (suffix() == "ecosystem") {

            if (period() == "1981-2010"){

              "richness"

            } else {

              input[[paste0("infos_", suffix())]]
            }

          } else {

            if (suffix() == "network") {

              if (period() == "1981-2010"){

                paste0("values_", suffix())

              } else {

                input[[paste0("infos_", suffix())]]
              }

            } else {

              "vulnerability"
            }
          }
        }
      }

    } else {

      NULL
    }
  })


  ### DEFINE LEAFLET LEGEND TITLE ----------------------------------------------

  leaflet_title <- reactive({

    if (!is.null(var_selected())) {

      if (suffix() == "climate") {

        if (information() == paste0("normals_", suffix())) {

          "Climate normals"

        } else {

          if (information() == paste0("uncertainties_", suffix())) {

            "Uncertainties"

          } else {

            "Anomalies"
          }
        }

      } else {

        if (suffix() == "species") {

          if (information() == paste0("observations_", suffix())) {

            "Observations"

          } else {

            if (information() == paste0("binaries_", suffix())) {

              "Presence/absence"

            } else {

              if (information() == paste0("probabilities_", suffix())) {

                "Probabilities"

              } else {

                "Uncertainties"
              }
            }
          }

        } else {

          if (suffix() == "ecosystem") {

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

          } else {

            if (suffix() == "network") {

              if (information() == paste0("values_", suffix())) {

                "Values"

              } else {

                "Anomalies"
              }

            } else {

              "Vulnerability index"
            }
          }
        }
      }

    } else {

      NULL
    }
  })


  ### DEFINE VARIABLE NAME (for exported map) ----------------------------------

  var_english <- reactive({

    if (!is.null(var_selected())) {

      if (suffix() %in% c("climate", "species", "network", "vulnerability")) {

        pos <- NULL

        for (i in columns()) {

          if (length(which(data()[ , i] == var_selected())) == 1) {

            pos <- which(data()[ , i] == var_selected())
          }
        }

        as.character(data()[pos, grep("^english$|^common_en$", colnames(data()))])

      } else {

        pos <- which(data()[ , "info"]  == information() & data()[ , "class"] == spclass())
        xxx <- data()[pos, "english"]

        if (length(grep(" \\(%\\)", xxx)) == 1) {

          xxx <- paste0("Percentage of ", gsub(" \\(%\\)", "", xxx))
          xxx <- gsub("S", "s", xxx)
        }

        paste0(xxx, " (", data()[pos, "class"], ")")
      }

    } else {

      NULL
    }
  })


  ### DEFINE LEGEND TITLE (for exported map) -----------------------------------

  legend_title <- reactive({

    if (!is.null(var_selected())) {

      if (suffix() == "climate") {

        if (information() == paste0("normals_", suffix())) {

          leg_suffix <- ""

        } else {

          if (information() == paste0("uncertainties_", suffix())) {

            leg_suffix <- " (Uncertainties)"

          } else {

            leg_suffix <- " (Anomalies)"
          }
        }

      } else {

        if (suffix() == "species") {

          if (information() == paste0("observations_", suffix())) {

            leg_suffix <- " (Observations)"

          } else {

            if (information() == paste0("binaries_", suffix())) {

              leg_suffix <- ""

            } else {

              if (information() == paste0("probabilities_", suffix())) {

                leg_suffix <- ""

              } else {

                leg_suffix <- " (Uncertainties)"
              }
            }
          }

        } else {

          if (suffix() == "network") {

            if (information() == paste0("anomalies_", suffix())) {

              leg_suffix <- " (Anomalies)"

            } else {

              leg_suffix <- ""
            }

          } else {

            leg_suffix <- ""
          }
        }
      }

      paste0(
        var_english(),
        ifelse(
          test = !is.null(var_units()),
          yes  = paste0(" (in ", var_units(), ") "),
          no   = " "
        ),
        period(),
        ifelse(
          !is.null(rcp()),
          paste0(" [", rcp_legend(), "]"),
          ""
        ),
        leg_suffix
      )

    } else {

      NULL
    }
  })


  ### DATA SOURCE (for exported map) -------------------------------------------

  data_source <- reactive({

    if (!is.null(var_selected())) {

      if (suffix() == "species") {

        if (information() == "observations_species") {

          if (spclass() == "Aves") {

            datasource <- "BirdLife International & NatureServe"

          } else {

            datasource <- "IUCN"
          }

        } else {

          datasource <- NULL
        }

      } else {

        if (suffix() == "climate") {

          if (period() == "1981-2010") {

            datasource <- "NCEP-CFSR Reanalysis"

          } else {

            datasource <- "CORDEX/Ouranos"
          }

        } else {

          NULL
        }
      }

    } else {

      NULL
    }
  })


  ### GET FILENAME SUFFIX (if required) ----------------------------------------

  filename_ext <- reactive({

    if (!is.null(var_selected())) {

      if (suffix() == "climate") {

        if (information() == paste0("normals_", suffix())) {

          "_mean"

        } else {

          if (information() == paste0("uncertainties_", suffix())) {

            "_sd"

          } else {

            "_diff"
          }
        }

      } else {

        if (suffix() == "species") {

          if (information() == paste0("observations_", suffix())) {

            "_observations"

          } else {

            if (information() == paste0("binaries_", suffix())) {

              "_binaries"

            } else {

              if (information() == paste0("probabilities_", suffix())) {

                "_probabilities"

              } else {

                "_uncertainties"
              }
            }
          }

        } else {

          if (suffix() == "network") {

            if (information() == paste0("anomalies_", suffix())) {

              "_diff"

            } else {

              ""
            }

          } else {

            ""
          }
        }
      }
    } else {

      NULL
    }
  })


  ### SET RASTER PATH ------------------------------------------------------

  raster_path <- reactive({

    if (!is.null(var_selected())) {

      if (suffix() %in% c("climate", "species", "network", "vulnerability")) {

        pos <- NULL

        for (i in columns()) {

          if (length(which(data()[ , i] == var_selected())) == 1) {

            pos <- which(data()[ , i] == var_selected())
          }
        }

      } else {

        pos <- which(data()[ , "info"]  == information() & data()[ , "class"] == spclass())
      }

      code <- data()[pos, "code"]

      if (period() == "1981-2010"){

        paste0(
          "data/",
          ifelse(
            test = suffix() == "species",
            yes  = tolower(spclass()),
            no   = ifelse(
              test = suffix() != "vulnerability",
              yes  = suffix(),
              no   = "network"
            )
          ),
          "/",
          code,
          "_",
          ifelse(
            test = suffix() == "species",
            yes  = period(),
            no   = gsub("-", "", period())
          ),
          filename_ext()
        )

      } else {

        paste0(
          "data/",
          ifelse(
            test = suffix() == "species",
            yes  = tolower(spclass()),
            no   = ifelse(
              test = suffix() != "vulnerability",
              yes  = suffix(),
              no   = "network"
            )
          ),
          "/",
          code,
          "_",
          ifelse(
            test = suffix() == "species",
            yes  = period(),
            no   = gsub("-", "", period())
          ),
          "_",
          tolower(gsub("\\.", "", rcp_legend())),
          filename_ext()
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
      if (filename_ext() == "_probabilities") {

        ras[][which(!is.na(ras[]))] <- ifelse(
          test = ras[][which(!is.na(ras[]))] > 1,
          yes  = 1,
          no   = ras[][which(!is.na(ras[]))]
        )
      }

      ras

    } else {

      NULL
    }
  })


  ### GET SELECTED DPI (for exported map) --------------------------------------

  dpi <- reactive({

    if (!is.null(var_selected())) {

      as.numeric(input[[paste0("dpi_", suffix())]])

    } else {

      NULL
    }
  })


  ### GET SELECTED MAP OUTPUT TYPE (for exported map) --------------------------

  type <- reactive({

    if (!is.null(var_selected())) {

      input[[paste0("format_", suffix())]]

    } else {

      NULL
    }
  })


  ### GET SELECTED RAMP COLOR --------------------------------------------------

  rampcolor <- reactive({

    if (!is.null(var_selected())) {

      input[[paste0("color_", suffix())]]

    } else {

      NULL
    }
  })


  ### CREATE COLOR GRADIENT (based on data) ------------------------------------

  couleurs <- reactive({

    if (!is.null(var_selected())) {

      # Color palette
      mycol <- brewer.pal(
        n    = rampcolors[which(rampcolors[ , "palette"] == gsub("-rev", "", rampcolor())), "maxcolors"],
        name = gsub("-rev", "", rampcolor())
      )

      # Reverse colors (if required)
      if (length(grep("-rev", rampcolor())) == 1) {

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


  ### UPDATE MAP / RESET MAP ---------------------------------------------------

  observe({


    ### UPDATE MAP -------------------------------------------------------------

    if (!is.null(var_selected())) {

      leafletProxy(
        mapId = paste0("map_", suffix()),
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
        title     = leaflet_title(),
        opacity   = 1,
        className = "info legend"
      )


    ### RESET MAP --------------------------------------------------------------

    } else {

      leafletProxy(
        mapId = paste0("map_", suffix()),
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
  })


  ### UPDATE LIST OF VARIABLES / SPECIES (select menus) ------------------------

  observe({

    if (suffix() %in% c("species", "climate", "network")) {

      updateSelectInput(
        session  = session,
        inputId  = paste0("select_", suffix()),
        label    = NULL,
        choices  = var_list(),
        selected = ifelse(!is.null(var_selected()), var_selected(), "")
      )
    }
  })


  ### ENABLE / DISABLE RCP RADIO BUTTONS ---------------------------------------

  observe({

    if (!is.null(period())) {

      # Disable
      if (period() == "1981-2010") {

        for (i in c("rcp45_", "rcp85_")) {

          shinyjs::disable(selector = paste0("[type=radio][value='", i, suffix(), "']"))
          shinyjs::runjs(paste0("$(\"[type=radio][value='", i, suffix(), "']\").prop(\"checked\", false);"))
        }

        shinyjs::runjs(paste0('$("#scenario_', suffix(), ' .shiny-options-group").css({"color": "#aaa"});'))
        shinyjs::runjs(paste0('$("#scenario_', suffix(), ' .shiny-options-group label").css({"cursor": "not-allowed"});'))

      # Enable
      } else {

        for (i in c("rcp45_", "rcp85_")) {

          shinyjs::enable(selector = paste0("[type=radio][value='", i, suffix(), "']"))
        }

        shinyjs::runjs(paste0('$("#scenario_', suffix(), ' .shiny-options-group").css({"color": "#555"});'))
        shinyjs::runjs(paste0('$("#scenario_', suffix(), ' .shiny-options-group label").css({"cursor": "pointer"});'))

        updateRadioButtons(
          session = session,
          inputId = paste0("scenario_", suffix()),
          selected = rcp()
        )
      }
    }
  })


  ### ENABLE / DISABLE INFORMATION RADIO BUTTON --------------------------------

  observe({

    if (suffix() == "climate") {

      xxx <- c("normals_", "anomalies_", "uncertainties_")

      if (period() == "1981-2010") {

        for (i in 2:3) {

          shinyjs::disable(selector = paste0("[type=radio][value='", xxx[i], suffix(), "']"))
          shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:nth-child(', i, ')").css({"color": "#aaa"});'))
          shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:nth-child(', i, ') label").css({"cursor": "not-allowed"});'))
          shinyjs::runjs(paste0("$(\"[type=radio][value='", xxx[i], suffix(), "']\").prop(\"checked\", false);"))
        }

      } else {

        for (i in 2:3) {

          shinyjs::enable(selector = paste0("[type=radio][value='", xxx[i], suffix(), "']"))
          shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:nth-child(', i, ')").css({"color": "#555"});'))
          shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:nth-child(', i, ') label").css({"cursor": "pointer"});'))
        }
      }

      updateRadioButtons(
        session  = session,
        inputId  = paste0("infos_", suffix()),
        selected = information()
      )

    } else {

      if (suffix() == "species") {

        if (period() == "1981-2010") {

          shinyjs::enable(selector = "[type=radio][value='observations_species']")
          shinyjs::runjs('$("#infos_species .shiny-options-group .radio:first-child").css({"color": "#555"});')
          shinyjs::runjs('$("#infos_species .shiny-options-group .radio:first-child label").css({"cursor": "pointer"});')

        } else {

          shinyjs::disable(selector = "[type=radio][value='observations_species']")
          shinyjs::runjs('$("#infos_species .shiny-options-group .radio:first-child").css({"color": "#aaa"});')
          shinyjs::runjs('$("#infos_species .shiny-options-group .radio:first-child label").css({"cursor": "not-allowed"});')
        }

        updateRadioButtons(
          session  = session,
          inputId  = paste0("infos_", suffix()),
          selected = information()
        )

      } else {

        if (suffix() == "ecosystem") {

          xxx <- c("richness", "gains", "losses", "turnover")

          if (period() == "1981-2010") {

            for (i in 2:4) {

              shinyjs::disable(selector = paste0("[type=radio][value='", xxx[i], "']"))
              shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:nth-child(', i, ')").css({"color": "#aaa"});'))
              shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:nth-child(', i, ') label").css({"cursor": "not-allowed"});'))
              shinyjs::runjs(paste0("$(\"[type=radio][value='", xxx[i], "']\").prop(\"checked\", false);"))
            }

          } else {

            for (i in 2:4) {

              shinyjs::enable(selector = paste0("[type=radio][value='", xxx[i], "']"))
              shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:nth-child(', i, ')").css({"color": "#555"});'))
              shinyjs::runjs(paste0('$("#infos_', suffix(), ' .shiny-options-group .radio:nth-child(', i, ') label").css({"cursor": "pointer"});'))
            }
          }

          updateRadioButtons(
            session  = session,
            inputId  = paste0("infos_", suffix()),
            selected = information()
          )

        } else {

          if (suffix() == "network") {

            if (period() == "1981-2010") {

              shinyjs::disable(selector = "[type=radio][value='anomalies_network']")
              shinyjs::runjs('$("#infos_network .shiny-options-group .radio:nth-child(2)").css({"color": "#aaa"});')
              shinyjs::runjs('$("#infos_network .shiny-options-group .radio:nth-child(2) label").css({"cursor": "not-allowed"});')

            } else {

              shinyjs::enable(selector = "[type=radio][value='anomalies_network']")
              shinyjs::runjs('$("#infos_network .shiny-options-group .radio:nth-child(2)").css({"color": "#555"});')
              shinyjs::runjs('$("#infos_network .shiny-options-group .radio:nth-child(2) label").css({"cursor": "pointer"});')
            }

            updateRadioButtons(
              session  = session,
              inputId  = paste0("infos_", suffix()),
              selected = information()
            )
          }
        }
      }
    }
  })


  ### ENABLE / DISABLE DPI OPTION IF PDF ---------------------------------------

  observe({

    if (!is.null(type())) {

      if (type() == "PDF") {

        shinyjs::disable(id = paste0("dpi_", suffix()))

      } else {

        shinyjs::enable(id = paste0("dpi_", suffix()))
      }
    }
  })


  ### SHOW / HIDE (ON CLICK) AVAILABLE COLOR PALETTES --------------------------

  observe({

    onclick(paste0("grad_", suffix()), function() {
      toggle(id = paste0("menu_", suffix()))
      toggleClass(id = paste0("grad_", suffix()), class = "shadow")
    })

    onclick(paste0("menu_", suffix()), function(){
      toggle(id = paste0("menu_", suffix()))
      toggleClass(id = paste0("grad_", suffix()), class = "shadow")
    })
  })


  ### CLICK ON INFORMATION BUTTON (main panel) ---------------------------------

  observe({

    onclick(paste0("help-", suffix()), function(){

      showModal(
        modalDialog(

          title = "Additional informations",

          withMathJax(helpText(includeHTML(paste0("includes/help-", suffix(), ".html")))),

          easyClose = TRUE,
          footer    = NULL
        )
      )
    })
  })


  ### CLICK ON DOWNLOAD BUTTON (main panel) ------------------------------------

  observe({

    onclick(paste0("btn-", suffix()), function(){


      ### SHOW DOWNLOAD PARAMETERS -------------------------------------------

      if (!is.null(var_selected())) {

        showModal(
          modalDialog(

            title = "Download options",

            HTML(paste0("<div id=\"save_", suffix(), "\">")),

            fluidRow(

              column(6,
                radioButtons(
                  inputId  = paste0("format_", suffix()),
                  label    = "Output format:",
                  choices  = c("PNG", "JPEG", "TIFF", "PDF"),
                  selected = "PNG",
                  inline   = FALSE
                )
              ),

              column(6,
                selectInput(
                  inputId  = paste0("dpi_", suffix()),
                  label    = "Resolution (dpi):",
                  choices  = c(72, 96, 150, 300, 600, 900),
                  selected = 300,
                  width    = 150
                )
              )
            ),

            HTML("<div class=\"bouton\">"),
            downloadButton(paste0("download_", suffix()), "Download map"),
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
                ifelse(
                  test = suffix() == "species",
                  yes  = "Please select a species.",
                  no   = "Please select a variable."
                ),
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
  })


  ### CLICK ON DOWNLOAD BUTTON (popup window) ----------------------------------

  for (btn in c("download_species", "download_climate", "download_ecosystem", "download_network", "download_vulnerability")) {

    output[[btn]] <- downloadHandler(

      filename =  function() {


        ### FILE NAME ----------------------------------------------------------

        paste0(
          "map-",
          gsub("[[:punct:]]|[[:space:]]", "", Sys.time()),
          ".",
          tolower(type())
        )
      },

      content  = function(file) {


        ### MESSAGE DURING MAP PREPARATION -------------------------------------

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


        ### MAP PRODUCTION -------------------------------------------------------

        if (suffix() %in% c("climate", "species")) {

          mapQuebec(
            x          = ras(),
            title      = legend_title(),
            type       = ifelse(leaflet_title() %in% c("Observations", "Presence/absence"), "binary", 1),
            datasource = data_source(),
            palette    = gsub("-rev", "", rampcolor()),
            reverse    = ifelse(length(grep("-rev", rampcolor())) == 1, TRUE, FALSE),
            bins       = 7
          )

        } else {

          mapTundra(
            x          = ras(),
            title      = legend_title(),
            type       = 1,
            datasource = NULL,
            palette    = gsub("-rev", "", rampcolor()),
            reverse    = ifelse(length(grep("-rev", rampcolor())) == 1, TRUE, FALSE),
            bins       = 7
          )
        }


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
}
