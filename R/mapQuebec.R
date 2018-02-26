mapQuebec <- function(x, title, datasource, palette = "Spectral", reverse = TRUE, bins = 7){


  ### MAP PARAMETERS -----------------------------------------------------------

  light <- "#cccccc"
  dark1 <- "#666666"
  dark2 <- "#aaaaaa"
  watin <- "#b4d1d1"
  watou <- "#85b4b4"


  par(
    xaxs   = "i",
    yaxs   = "i",
    family = "serif",
    mar    = c(4.50, 1.70, 0.75, 1.70),
    new    = FALSE,
    xpd    = FALSE
  )


  ### MAP EXTENT ---------------------------------------------------------------

  plot(readRDS("data/background/study.rds"),
    xlim   = c(-80.50, -55.25),
    ylim   = c( 50.50,  63.50),
    axes   = FALSE,
    col    = light,
    border = "transparent"
  )

  showNotification("Add study")



  ### ADD RASTER ---------------------------------------------------------------

  x <- projectRaster(
    from   = x,
    method = "ngb",
    crs    = "+init=epsg:4326"
  )

  showNotification("Project raster")

  plotRVB(x,
    palette = palette,
    reverse = reverse,
    add     = TRUE
  )

  showNotification("Plot raster")


  ### ADD BACKGROUND LAYERS ----------------------------------------------------

  plot(
    readRDS("data/background/nunavut.rds"),
    add = TRUE, col = light, border = "transparent"
  )
  showNotification("Add nunavut")
  plot(
    readRDS("data/background/ontario.rds"),
    add = TRUE, col = light, border = dark2
  )
  showNotification("Add Ontario")
  plot(
    readRDS("data/background/noquebec.rds"),
    add = TRUE, col = light, border = dark2
  )
  showNotification("Add No Quebec")
  plot(
    readRDS("data/background/nolabrador.rds"),
    add = TRUE, col = light, border = dark2
  )
  showNotification("Add No Labrador")
  plot(
    readRDS("data/background/labrador.rds"),
    add = TRUE, col = "transparent", border = dark2
  )
  showNotification("Add Labrador")
  plot(
    readRDS("data/background/inland-water.rds"),
    add = TRUE, col = watin, border = watou
  )
  showNotification("Add Inland Water")
  plot(
    readRDS("data/background/study.rds"),
    add = TRUE, col = "transparent", border = dark1
  )
  showNotification("Add Study")
  plot(
    readRDS("data/background/ocean.rds"),
    add = TRUE, col = watin, border = watou
  )
  showNotification("Add Ocean")
  plot(
    readRDS("data/background/graticules.rds"),
    add = TRUE, col = watou
  )
  showNotification("Add Graticules")
  plot(
    readRDS("data/background/eco-tundra.rds"),
    add = TRUE, col = "transparent", border = "white", lwd = 3
  )
  showNotification("Add Tundra 1")
  plot(
    readRDS("data/background/eco-tundra.rds"),
    add = TRUE, col = "transparent", border = dark1, lwd = 1.75
  )
  showNotification("Add Tundra 2")


  ### ADD TOPONYMY ITEMS -------------------------------------------------------

  text(
    labels = "Ungava\nBay",
    x = -67.56, y = 59.42, font = 3, col = watou, cex = 0.65
  )
  text(
    labels = "Hudson\nBay",
    x = -78.97, y = 58.00, font = 3, col = watou, cex = 0.65
  )
  text(
    labels = "Labrador\nSea",
    x = -57.56, y = 58.13, font = 3, col = watou, cex = 0.65
  )
  text(
    labels = "Hudson Strait",
    x = -72.50, y = 62.85, font = 3, col = watou, cex = 0.65, srt = -45
  )
  text(labels = "Gulf of St Lawrence",
    x = -60.55, y = 49.85, font = 3, col = watou, cex = 0.65
  )
  showNotification("Add Text")

  ### ADD X-AXIS ---------------------------------------------------------------

  options(warn = -1)

  for (side in c(1, 3)) {

    if (side == 1) par(mgp = c(3.00, -0.15, 0.00))
    if (side == 3) par(mgp = c(3.00,  0.25, 0.00))

    axis(
      side      = side,
      at        = seq(-80, -55, by = 5),
      labels    = paste0(-1 * seq(-80, -55, by = 5), "°W"),
      lwd       = 0,
      lwd.ticks = 1,
      col.axis  = dark1,
      col       = dark1,
      cex.axis  = 0.65,
      tck       = -0.01
    )
  }
  showNotification("Add x-Axes")


  ### ADD Y-AXIS ---------------------------------------------------------------

  for (side in c(2, 4)) {

    if (side == 2) par(mgp = c(3.00,  0.35, 0.00))
    if (side == 4) par(mgp = c(3.00, -0.15, 0.00))

    axis(
      side      = side,
      at        = seq(48, 64, by = 4),
      labels    = paste0(1 * seq(48, 64, by = 4), "°N"),
      lwd       = 0,
      lwd.ticks = 1,
      col.axis  = dark1,
      col       = dark1,
      cex.axis  = .65,
      tck       = -0.01
    )
  }
  showNotification("Add y-Axes")

  options(warn = 0)


  ### ADD NORTH ARROW ----------------------------------------------------------

  # rasterImage(
  #   image   = readPNG("data/background/north-arrow.png"),
  #   xleft   = -59.50,
  #   ybottom =  61.50,
  #   xright  = -56.00,
  #   ytop    =  63.50,
  #   col     = "red"
  # )
  # showNotification("Add North Arrow")

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
  showNotification("Create Legend Plot")


  ### GET LEGEND CHARACTERISTICS -----------------------------------------------

  legende <- getLegend(x, palette = palette, reverse = reverse, bins = bins)

  p       <- legende$p
  cuts    <- legende$cuts
  colors  <- legende$colors
  ncolors <- length(colors)

  showNotification("Compute Legend")


  ### ADD LEGEND COLORS --------------------------------------------------------

  for (i in 1:ncolors) {
    rect(
      xleft   = xmin + xrng * ((i - 1) / ncolors),
      xright  = xmin + xrng * (i / ncolors),
      ytop    = yctr + ywdt / 2,
      ybottom = yctr - ywdt / 2,
      col     = colors[i],
      border  = NA,
      lty     = 1,
      lwd     = 1
    )
  }
  showNotification("Add Legend Colors")


  ### ADD LEGEND FRAME ---------------------------------------------------------

  rect(
    xleft   = xmin,
    xright  = xmax,
    ytop    = yctr + ywdt / 2,
    ybottom = yctr - ywdt / 2,
    col     = NA,
    border  = dark1,
    lwd     = 1,
    lty     = 1
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
      cex   = .7,
      font  = 1
    )
  }

  showNotification("Add Legend labels")


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
