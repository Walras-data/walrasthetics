# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

walras_colors <- c(
  `white`        = "#FDFFFF",
  `redao`       = "#4E0A03",
  `grey`      = "#CFD1D1",
  `bluezinho`   = "#72B1BD",
  `bluezao`     = "#005176",
  `redinho`     = "#FE635E",
  `amarelo` = "#E4B363",
  `preto` = "#313638")

#' Function to extract drsimonj colors as hex codes
#'
#' @param ... Character names of drsimonj_colors
#'
walras_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (drsimonj_colors)

  walras_colors[cols]
}



walras_palettes <- list(
  `main`  = walras_cols("redao", "bluezao"),

  `cool`  = walras_cols("bluezinho", "redinho"),

  `mixed` = walras_cols("preto", "amarelo", "bluezinho", "redinho", "grey")
)



#' Return function to interpolate a drsimonj color palette
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
walras_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- walras_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}






#' Color scale constructor for walras colors
#'
#' @param palette Character name of palette in walras_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_walras <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- walras_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("walras_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for walras colors
#'
#' @param palette Character name of palette in walras_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_walras <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- walras_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("walras_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
