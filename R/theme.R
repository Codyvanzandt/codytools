# Sequential ramp used for ordered factors and binned fills.
cody_ramp <- grDevices::colorRampPalette(c("#E6D3CF", "#D4B3B7", "#B47885", "#873B47", "#48202C"))

# Discrete palette that never drops categories: past six, extend with lighter tints.
cody_discrete <- function(n) {
  if (n <= 6) return(cody_palette[seq_len(n)])
  message("More than 6 categories: extra colours are lighter tints; consider facets or direct labels.")
  tints <- scales::col_mix(cody_palette, cody_colours[["paper"]], 0.45)
  rep_len(c(cody_palette, tints), n)
}

#' The Cody theme: type, layout, geoms and default palettes in one place.
theme_cody <- function(base_size = 11, ...) {
  col <- cody_colours
  theme_cody_base(base_size = base_size, ...) + ggplot2::theme(
    geom = ggplot2::element_geom(ink = col[["ink"]], paper = col[["paper"]], accent = col[["burgundy"]],
                                 linewidth = 0.9, borderwidth = 0.5, pointsize = 2.3, pointshape = 16),
    palette.colour.discrete = cody_discrete, palette.fill.discrete = cody_discrete,
    palette.colour.continuous = cody_sequential, palette.fill.continuous = cody_sequential,
    palette.shape.discrete = function(n) cody_shapes[seq_len(n)],
    palette.linetype.discrete = function(n) cody_linetypes[seq_len(n)],
    axis.text = ggplot2::element_text(size = base_size * .92, colour = col[["muted"]]),
    legend.location = "plot", legend.justification = "left",
    legend.box.spacing = grid::unit(2, "pt"), legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.title = ggplot2::element_text(size = base_size * .85, colour = col[["muted"]]),
    legend.key.width = grid::unit(2, "lines"), legend.key.height = grid::unit(.8, "lines"),
    legend.text = ggplot2::element_text(size = base_size * .9))
}

# Everything that must be switched on for plots to "just work": fonts, theme,
# ordinal/binned defaults, geom defaults, and the editorial knitr renderer.
cody_use <- function(base_size = 11, ...) {
  cody_register_fonts()
  ggplot2::theme_set(theme_cody(base_size = base_size, ...))
  options(
    ggplot2.ordinal.fill = function(...) ggplot2::discrete_scale("fill", palette = cody_ramp, ...),
    ggplot2.ordinal.colour = function(...) ggplot2::discrete_scale("colour", palette = cody_ramp, ...))
  upd <- function(geom, ...) try(ggplot2::update_geom_defaults(geom, ggplot2::aes(...)), silent = TRUE)
  upd("point", colour = ggplot2::from_theme(accent))
  upd("line", colour = ggplot2::from_theme(accent))
  upd("col", fill = ggplot2::from_theme(accent))
  upd("bar", fill = ggplot2::from_theme(accent))
  upd("tile", colour = ggplot2::from_theme(paper), linewidth = 0.5)
  upd("boxplot", colour = ggplot2::from_theme(scales::col_mix(ink, paper, .25)))
  upd("density", colour = ggplot2::from_theme(scales::col_mix(ink, paper, .45)), linewidth = 0.5)
  if (requireNamespace("ggdist", quietly = TRUE))
    try(ggplot2::update_geom_defaults(ggdist::GeomSlabinterval, ggplot2::aes(
      fill = ggplot2::from_theme(scales::col_mix(accent, paper, .55)),
      colour = ggplot2::from_theme(ink))), silent = TRUE)
  if (isTRUE(getOption("knitr.in.progress")))
    knitr::opts_chunk$set(render = cody_knit_print)
  invisible()
}
