mapTundra <- function(x, title, type = NULL, datasource, palette = "Spectral", reverse = TRUE, bins = 7){


  if (!is.null(type)) { if (type == 1) { type <- NULL } }


  ### MAP PARAMETERS -----------------------------------------------------------

  light <- "#cccccc"
  dark1 <- "#666666"
  dark2 <- "#aaaaaa"
  watin <- "#b4d1d1"
  watou <- "#85b4b4"
  cache <- "#33333355"


  par(
    xaxs   = "i",
    yaxs   = "i",
    family = "serif",
    mar    = c(4.50, 1.70, 0.75, 1.70),
    new    = FALSE,
    xpd    = FALSE
  )


  ### MAP EXTENT ---------------------------------------------------------------

  ext <- as(extent(c(-80.0, -67, 56.0, 63.5)), "SpatialPolygons")
  proj4string(ext) <- CRS("+init=epsg:4326")

  plot(ext,
    axes   = FALSE,
    col    = "transparent",
    border = "transparent"
  )

  plot(readRDS("data/background/quebec.rds"),
    add    = TRUE,
    border = "transparent",
    col    = "white"
  )

  plot(readRDS("data/background/eco-tundra.rds"),
    add    = TRUE,
    border = "transparent",
    col    = light
  )

  plot(readRDS("data/background/eco-tundra.rds"),
    add    = TRUE,
    border = "transparent",
    col    = cache,
    density = 20,
    angle = 45
  )


  ### ADD RASTER ---------------------------------------------------------------

  x <- projectRaster(
    from   = x,
    method = "ngb",
    crs    = "+init=epsg:4326"
  )

  ccols <- plotRVB(x,
    type    = type,
    palette = palette,
    reverse = reverse,
    add     = TRUE
  )


  ### ADD BACKGROUND LAYERS ----------------------------------------------------

  plot(
    readRDS("data/background/nunavut.rds"),
    add = TRUE, col = light, border = "transparent"
  )

  plot(
    readRDS("data/background/noquebec.rds"),
    add = TRUE, col = light, border = "transparent"
  )

  plot(
    readRDS("data/background/inland-water.rds"),
    add = TRUE, col = watin, border = "transparent"
  )

  plot(
    readRDS("data/background/ocean-na.rds"),
    add = TRUE, col = watin, border = watou
  )

  plot(
    readRDS("data/background/graticules-na.rds"),
    add = TRUE, col = watou, border = par()$fg
  )

  plot(
    readRDS("data/background/eco-tundra.rds"),
    add = TRUE, col = "transparent", border = "white", lwd = 3
  )

  plot(
    readRDS("data/background/eco-tundra.rds"),
    add = TRUE, col = "transparent", border = dark1, lwd = 1.75
  )


  ### ADD TOPONYMY ITEMS -------------------------------------------------------

  text(
    labels = "Ungava Bay",
    x = -67.70, y = 59.42, font = 3, col = watou, cex = 0.65
  )
  text(
    labels = "Hudson Bay",
    x = -79.15, y = 58.10, font = 3, col = watou, cex = 0.65
  )
  text(
    labels = "Hudson Strait",
    x = -70.50, y = 62.00, font = 3, col = watou, cex = 0.65, srt = -45
  )


  ### ADD X-AXIS ---------------------------------------------------------------

  options(warn = -1)

  for (side in c(1, 3)) {

    if (side == 1) par(mgp = c(3.00, -0.15, 0.00))
    if (side == 3) par(mgp = c(3.00,  0.25, 0.00))

    axis(
      side      = side,
      at        = seq(-81, -66, by = 3),
      labels    = paste0(-1 * seq(-81, -66, by = 3), "°W"),
      lwd       = 0,
      lwd.ticks = 1,
      col.axis  = dark1,
      col       = dark1,
      cex.axis  = 0.65,
      tck       = -0.01
    )
  }


  ### ADD Y-AXIS ---------------------------------------------------------------

  for (side in c(2, 4)) {

    if (side == 2) par(mgp = c(3.00,  0.35, 0.00))
    if (side == 4) par(mgp = c(3.00, -0.15, 0.00))

    axis(
      side      = side,
      at        = seq(57, 63, by = 2),
      labels    = paste0(1 * seq(57, 63, by = 2), "°N"),
      lwd       = 0,
      lwd.ticks = 1,
      col.axis  = dark1,
      col       = dark1,
      cex.axis  = .65,
      tck       = -0.01
    )
  }

  options(warn = 0)


  ### ADD MAP FRAME ------------------------------------------------------------

  box("plot", col = dark1)


  ### LEGEND PARAMETERS --------------------------------------------------------

  xmin <- -1 ; xmax <- +1
  xrng <- abs(xmax - xmin)

  ymin <- -1 ; ymax <- +1
  yrng <- abs(ymax - ymin)

  yctr <- ((ymax + ymin) / 2) + .1
  ywdt <- 1

  par(new = TRUE, mar = c(0.95, 1.70, 27.00, 1.70), xpd = TRUE)

  plot(0,
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax),
    type = "n",
    axes = FALSE,
    ann  = FALSE
  )


  ### CLASSICAL LEGEND (CONTINUOUS DATA) ---------------------------------------

  if (is.null(type)) {

    ### GET LEGEND CHARACTERISTICS -----------------------------------------------

    legende <- getLegend(x, palette = palette, reverse = reverse, bins = bins)

    p       <- legende$p
    cuts    <- legende$cuts
    colors  <- legende$colors
    ncolors <- length(colors)


    ### ADD LEGEND COLORS --------------------------------------------------------

    for (i in 1:ncolors) {
      rect(
        xleft   = xmin + xrng * ((i - 1) / ncolors),
        xright  = xmin + xrng * (i / ncolors),
        ytop    = yctr + ywdt / 2,
        ybottom = yctr - ywdt / 2,
        col     = colors[i],
        border  = NA
      )
    }


    ### ADD LEGEND FRAME ---------------------------------------------------------

    rect(
      xleft   = xmin,
      xright  = xmax,
      ytop    = yctr + ywdt / 2,
      ybottom = yctr - ywdt / 2,
      col     = NA,
      border  = dark1
    )


    ### ADD LEGEND LABELS --------------------------------------------------------

    for (i in 1:length(p)) {

      lines(
        x   = rep(xmin + xrng * p[i], 2),
        y   = c(yctr - ywdt / 2, (yctr - ywdt / 2) - yrng * 0.10),
        col = dark1
      )
      text(
        x     = xmin + (2 * p[i]),
        y     = (yctr - ywdt / 2) - yrng * 0.30,
        label = format(cuts)[i],
        col   = dark1,
        cex   = 0.7
      )
    }


  ### SPECIAL LEGEND (BINARY DATA) ---------------------------------------------

  } else {

    rect(
      xleft   = -0.55,
      ybottom = (yctr - 0.5) - ywdt / 2,
      xright  = -0.45,
      ytop    = (yctr - 0.5) + ywdt / 2,
      col     = ccols[1],
      border  = dark1
    )
    text(
      x     = -0.45,
      y     = (((yctr - 0.5) - ywdt / 2) + ((yctr - 0.5) + ywdt / 2)) / 2,
      label = "Absence",
      col   = dark1,
      cex   = 0.7,
      pos   = 4,
      font  = 2
    )

    rect(
      xleft   = 0.15,
      ybottom = (yctr - 0.5) - ywdt / 2,
      xright  = 0.25,
      ytop    = (yctr - 0.5) + ywdt / 2,
      col     = ccols[2],
      border  = dark1
    )
    text(
      x     = 0.25,
      y     = (((yctr - 0.5) - ywdt / 2) + ((yctr - 0.5) + ywdt / 2)) / 2,
      label = "Presence",
      col   = dark1,
      cex   = 0.7,
      pos   = 4,
      font  = 2
    )
  }


  ### ADD LEGEND TITLE ---------------------------------------------------------

  text(
    x      = (xmax + xmin) / 2,
    y      = 1.25,
    labels = title,
    col    = dark1,
    font   = 2,
    cex    = 0.80
  )


  ### ADD CREDITS --------------------------------------------------------------

  if (!is.null(datasource)) {

    text(
      x      = -1.05,
      y      = -1.85,
      labels = paste("Source:", datasource),
      col    = dark1,
      font   = 3,
      cex    = 0.55,
      pos    = 4
    )

    text(
      x      =  0.170,
      y      = -1.85,
      labels = "License CC-BY",
      col    = dark1,
      font   = 3,
      cex    = 0.55,
      pos    = 2
    )

  } else {

    text(
      x      = -1.05,
      y      = -1.85,
      labels = "License CC-BY",
      col    = dark1,
      font   = 3,
      cex    = 0.55,
      pos    = 4
    )
  }

  text(
    x      =  1.05,
    y      = -1.85,
    labels = "Citation: Berteaux et al. 2018",
    col    = dark1,
    font   = 3,
    cex    = 0.55,
    pos    = 2
  )
}
