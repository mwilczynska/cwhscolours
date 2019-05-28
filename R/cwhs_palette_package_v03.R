# CWHS colour palette
# Adapted from https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2

# Centre for WHS "Online" colour codes (p.12 of the CWHS brand guidlines):
cwhs_colours <- c(
  `slate`     = "#415058",
  `blue`      = "#83d3dc",
  `green`     = "#5dc7a3",
  `yellow`    = "#f7db9c",
  `grey`      = "#b4b4be")

#' Function to extract cwhs_online colours as hex codes
#'
#' @param ... Character names of cwhs_online colours
#'
cwhs_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (cwhs_colours)

  cwhs_colours[cols]
}

# This allows us to get hex colors in a robust and flexible way. For example, you can have all colors
# returned as they are, specify certain colors, in a particular order, add additional function arguments
# and checks, and so on.

# Combine colours into palettes
# So we can now create  palettes (various combinations) of these colors. Similar to how we deal with colors,
# first define a list like such:
cwhs_palettes <- list(
  `online`  = cwhs_cols("slate", "blue", "green", "yellow", "grey"),
  `cool` = cwhs_cols("blue", "green"),
  `contrast` = cwhs_cols("slate", "yellow"),
  `colours` = cwhs_cols("blue", "green", "yellow")
)

# Changes or new color palettes are added in this list. We write a function to access and interpolate them like so:
#' Function to extract cwhs_online colours as hex codes
#'
#' @param ... Character names of cwhs_online colours
#'
cwhs_pal <- function(palette = "online", reverse = FALSE, ...) {
  pal <- cwhs_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

# This function gets a pallete by name from the list ("main" by default), has a boolean condition determining
# whether to reverse the order or not, and additional arguments to pass on to colorRampPallete() (such as an
# alpha value). This returns another function:
#cwhs_pal("online")
# function (n)
# {
#   x <- ramp(seq.int(0, 1, length.out = n))
#   if (ncol(x) == 4L)
#     rgb(x[, 1L], x[, 2L], x[, 3L], x[, 4L], maxColorValue = 255)
#   else rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255)
# }
# <bytecode: 0x40b0c58>
#   <environment: 0x6167ad8>

# This returned function will interpolate the palette colors for a certain number of levels, making it possible
# to create shades between our original colors. To demonstrate, we can interpolate the "online" palette (which only
# includes five colours) to a length of 10:
#cwhs_pal("online")(10)
#  [1] "#415058" "#5E8A92" "#7BC4CD" "#76CFC9" "#65C9AF" "#7FCBA1" "#C3D49E" "#EFD69F" "#D1C5AE" "#B4B4BE"
# This is what we need to create custom scales in ggplot2

# Previous plot with the 4 cwhs_online colours
#ggplot(mtcars, aes(hp, mpg)) +
#  geom_point(color = cwhs_pal("online")(32),
#             size = 4, alpha = .8)

# Scales for ggplot2
# We've now got the essentials to create custom color and fill scale functions for ggplot2. There are many ways to do
# this. I like the approach taken in packages like ochRe. One function is created for color and another for fill, and
# each contains a boolean argument for the relevant aesthetic being discrete or not. Here are my versions:
#' Color scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in cwhs_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_colour_cwhs <- function(palette = "online", discrete = TRUE, reverse = FALSE, ...) {
  pal <- cwhs_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("cwhs_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Color scale constructor for CHWS colors
#'
#' @param palette Character name of palette in cwhs_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_cwhs <- function(palette = "online", discrete = TRUE, reverse = FALSE, ...) {
  pal <- cwhs_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("cwhs_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for CWHS colors
#'
#' @param palette Character name of palette in cwhs_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_cwhs <- function(palette = "online", discrete = TRUE, reverse = FALSE, ...) {
  pal <- cwhs_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("cwhs_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

# Each of these functions specifies a palette, whether the palette is being applied based on a discrete or
# numeric variable, whether to reverse the palette colors, and additional arguments to pass to the relevant
# ggplot2 function (which differs for discrete or numeric mapping).
