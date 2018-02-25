plotRVB <- function(
  x       = NULL,
  zlim    = NULL,
  palette = "Spectral",
  reverse = TRUE,
  alpha   = 255,
  bgalpha = 0,
  colNA   = NA,
  axes    = FALSE,
  xlab    = "",
  ylab    = "",
  add     = FALSE
) {


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


  # Reverse Palette if required ------------------------------------------------

  if (reverse) pal <- pal[length(pal):1]


  # Create colors gradient based on raster values ------------------------------

  hexa <- leaflet::colorNumeric(
    palette  = pal,
    domain   = values(x),
    na.color = NA
  )
  hexas <- hexa(values(x))


  # Convert Hexa to RBG --------------------------------------------------------

  rgbs  <- grDevices::col2rgb(hexas)


  # Create a 3-bands RGB RasterStack -------------------------------------------

  red <- green <- blue <- x

  raster::values(red)[cells]   <- rgbs[1, cells]
  raster::values(green)[cells] <- rgbs[2, cells]
  raster::values(blue)[cells]  <- rgbs[3, cells]

  x <- stack(red, green, blue)


  # Plot the raster ------------------------------------------------------------

  plotRGB(
    x, r = 1, g = 2, b = 3,
    zlim    = zlim,
    alpha   = alpha,
    bgalpha = bgalpha,
    colNA   = colNA,
    axes    = axes,
    xlab    = xlab,
    ylab    = ylab,
    add     = add
  )
}
