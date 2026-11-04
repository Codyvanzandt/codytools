# Diverging scales: choose these on purpose for values centred on zero.
scale_fill_cody_div <- function(..., midpoint = 0) scale_fill_cody_c(..., diverging = TRUE, midpoint = midpoint)
scale_colour_cody_div <- function(..., midpoint = 0) scale_colour_cody_c(..., diverging = TRUE, midpoint = midpoint)

# Keep a category's colour fixed across figures, naming colours instead of hex codes:
#   scale_colour_cody(values = c(Control = "burgundy", Treatment = "blue"))
cody_resolve <- function(values) {
  hit <- values %in% names(cody_colours)
  values[hit] <- unname(cody_colours[values[hit]])
  values
}
scale_colour_cody <- function(..., values) {
  ggplot2::scale_colour_manual(..., values = cody_resolve(values), na.value = cody_colours[["missing"]])
}
scale_fill_cody <- function(..., values) {
  ggplot2::scale_fill_manual(..., values = cody_resolve(values), na.value = cody_colours[["missing"]])
}
