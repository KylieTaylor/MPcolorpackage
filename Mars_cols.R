
#' Corporate Colors
#'
#' This is simply a list of the web color codes.
#'
#'
#' @export
Mars_colors <- c(
  `blue` = "#0000a0",
  `teal` = "#00d7b9",
  `yellow` = "#ffdc00",
  `skyblue` = "#00dcfa",
  `green` = "#a6db00",
  `purple` = "#9600ff",
  `pink` = "#ff32a0",
  `red` = "#ff3c14",
  `orange` = "#ff8200",
  `grey` = "#3c3c3c",
  `blue50` = "#8080cf",
  `teal50` = "#80ebdc",
  `yellow50` = "#ffed80",
  `skyblue50` = "#80edfc",
  `green50` = "#d2ed80",
  `purple50` = "#ca80ff",
  `pink50` = "#ff98cf",
  `red50` = "#ff9d89",
  `orange50` = "#ffc080",
  `white` = "#ffffff"
)


#' Call specified color
#'
#' This function outputs the name and web code for a called color.
#' It can be used in ggplot graph code to use discrete colors. There are
#' a total of 20 colors in the list. Options are "blue", "teal", "yellow",
#' "skyblue", "green", "purple", "pink", "red", "orange", "grey", "blue50",
#' "teal50", "yellow50", "skyblue50","green50", "purple50", "pink50",
#' "red50", "orange50", and "white".
#'
#'
#'
#' @param color Name of a color, or colors in the Mars_cols list
#' @return Name and web code of the specified color
#' @export
Mars_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (Mars_colors)

  Mars_colors[cols]
}


#' Palettes
#'
#' This is a list of various palettes generated using the 20
#' colors.
#' Palettes include "all", "cool", "warm", "solids", and "hues".
#'
#'
#' @export
Mars_palettes <- list(
  `all`  = Mars_cols("blue", "teal", "yellow", "skyblue", "green", "purple", "pink",
                     "red", "orange", "grey", "blue50", "teal50", "yellow50", "skyblue50",
                     "green50", "purple50", "pink50", "red50", "orange50", "white"),

  `cool`  = Mars_cols("blue", "teal", "skyblue", "green", "purple", "grey", "blue50",
                      "teal50", "skyblue50","green50", "purple50", "white"),

  `warm`   = Mars_cols("yellow", "pink", "red", "orange", "red", "yellow50", "pink50",
                       "red50", "orange50", "white"),

  `solids` = Mars_cols("blue", "teal", "yellow", "skyblue", "green", "purple", "pink",
                       "red", "orange", "grey", "white"),

  `hues` = Mars_cols("blue50", "teal50", "yellow50", "skyblue50",
                     "green50", "purple50", "pink50", "red50", "orange50", "white")
)



#' Call specified color palette
#'
#' This function outputs the name and web code for a called color palette.
#' It reverses the order of the palatte as well, so gradients can be made of
#' each palette.
#' Palettes include "all", "cool", "warm", "solids", and "hues".
#'
#'
#' @param palette Name of a color palette
#' @return Name and web code of the specified color palette
#' @export
Mars_pal <- function(palette = "all", reverse = FALSE, ...) {
  pal <- Mars_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}


#' Scale color for ggplot inputs
#'
#' This function creates a discrete and continuous color scale
#' of a specified palette to use in ggplot code.
#' Palettes include "all", "cool", "warm", "solids", and "hues".
#'
#'
#' @param palette Name of a Mars color palette
#' @return Scale color
#' @export
scale_color_Mars <- function(palette = "all", discrete = TRUE, reverse = FALSE, ...) {
  pal <- Mars_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("Mars_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}


#' Scale fill for ggplot inputs
#'
#' This function creates a scale fill of a specified
#' palette to use in ggplot code.
#' Palettes include "all", "cool", "warm", "solids", and "hues".
#'
#'
#' @param palette Name of a Mars color palette
#' @return Scale fill
#' @export
scale_fill_Mars <- function(palette = "all", discrete = TRUE, reverse = FALSE, ...) {
  pal <- Mars_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("Mars_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}
