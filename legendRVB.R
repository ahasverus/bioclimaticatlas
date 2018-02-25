getLegend <- function(x, palette = "Spectral", reverse = TRUE, bins = 7){

  if (is.null(x)) {
    stop("Single spatial raster layer is required.")
  }
  if (class(x) != "RasterLayer") {
    stop("Single spatial raster layer is required.")
  }


  # List of Brewer Palettes ----------------------------------------------------

  rampcolors <- data.frame(
    palette          = rownames(brewer.pal.info),
    maxcolors        = brewer.pal.info[ , "maxcolors"],
    stringsAsFactors = FALSE
  )


  # Check Palette --------------------------------------------------------------

  pos <- which(tolower(rampcolors[ , "palette"]) == tolower(palette))
  if (length(pos) == 0) {
    stop("Wrong palette name. See brewer.pal.info for a list of available palettes.")
  }


  # Identify non-NA cells ------------------------------------------------------

  cells <- which(!is.na(x[]))


  # Get Colors defining palette (from 8 to 12) ---------------------------------

  pal <- RColorBrewer::brewer.pal(
    n    = rampcolors[pos, "maxcolors"],
    name = gsub("-rev", "", palette)
  )


  # Reverse Palette if required  -----------------------------------------------

  if (reverse) pal <- pal[length(pal):1]


  # Create colors gradient based on raster values ------------------------------

  ramp <- leaflet::colorNumeric(
    palette  = pal,
    domain   = values(x),
    na.color = NA
  )


  # Define labels --------------------------------------------------------------

  cuts   <- pretty(values(x), n = bins)
  rng    <- range(values(x), na.rm = TRUE)
  cuts   <- cuts[cuts >= rng[1] & cuts <= rng[2]]
  n      <- length(cuts)
  p      <- (cuts - rng[1])/(rng[2] - rng[1])

  colors <- ramp(c(r[1], cuts, r[2]))


  # Create colors gradient for legend ------------------------------------------

  maxColors <- 255 # !important
  pp        <- c(0, p, 1)

  hexas <- NULL
  for (i in 2:(n + 2)) {
    hexas <- c(hexas, colorRampPalette(c(colors[c(i - 1, i)]))(round(maxColors * (pp[i] - pp[(i - 1)]))))
  }

  hexas <- hexas[which(!duplicated(hexas))]

  list(
    colors = hexas,
    cuts   = cuts,
    p      = p
  )
}
