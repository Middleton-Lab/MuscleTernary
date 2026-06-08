#' geom for labeling geom_points
#'
#' See: http://stackoverflow.com/q/19694497/168137
#'
#' @param mapping The aes mapping
#' @param data The data
#' @param stat The stat
#' @param position The position
#' @param parse Parse or not
#' @param na.rm Remove NAs silently if TRUE
#' @param show.legend Whether to show in legend
#' @param inherit.aes Whether to inherit aesthetics
#' @param ... Additional options passed on
#'
#' @return A geom usable by ggplot()
#'
#' @export
#'
#' @examples
#' \dontrun{
#' geom_shiftedtext(ggplot2::aes(label = muscle))
#' }
geom_shiftedtext <- function(mapping = NULL,
                              data = NULL,
                              stat = "identity",
                              position = "identity",
                              parse = FALSE,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE,
                              ...) {

  # Register GeomShiftedtext with ggtern's approved geom list
  tryCatch(
    utils::assignInNamespace(
      ".approvedgeom",
      c(get(".approvedgeom", envir = asNamespace("ggtern")),
        shiftedtext = "GeomShiftedtext"),
      "ggtern"
    ),
    error = function(e) invisible(NULL)
  )

  ggplot2::layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomShiftedtext,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(parse = parse, na.rm = na.rm, ...)
  )
}

GeomShiftedtext <- ggplot2::ggproto(
  "GeomShiftedtext",
  ggplot2::GeomText,
  draw_panel = function(data, panel_params, coord,
                        parse = FALSE,
                        na.rm = FALSE,
                        check_overlap = FALSE) {
    data <- data[
      !is.na(data$x) & !is.na(data$y) & !is.na(data$label),
    ]
    if (nrow(data) == 0L) return(grid::nullGrob())

    coords <- coord$transform(data, panel_params)
    lab <- coords$label
    if (parse) lab <- parse(text = lab)

    grid::textGrob(
      lab,
      grid::unit(coords$x, "npc") +
        grid::unit(0.375 * coords$size, "mm"),
      grid::unit(coords$y, "npc"),
      hjust    = coords$hjust,
      vjust    = coords$vjust,
      rot      = coords$angle,
      gp = grid::gpar(
        col        = ggplot2::alpha(coords$colour, coords$alpha),
        fontsize   = coords$size * ggplot2::.pt,
        fontfamily = coords$family,
        fontface   = coords$fontface,
        lineheight = coords$lineheight
      )
    )
  }
)
