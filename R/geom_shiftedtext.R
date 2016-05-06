#' geom for labeling geom_points
#'
#' See: http://stackoverflow.com/q/19694497/168137
#'
#' @param mapping The aes mapping
#' @param data The data
#' @param stat The stat
#' @param position The position
#' @param parse Parse or not
#' @param ... Additional options passed on
#'
#' @return A geom usable by ggplot()
#'
#' @export
#'
geom_shiftedtext <- function (mapping = NULL,
                              data = NULL,
                              stat = "identity",
                              position = "identity",
                              parse = FALSE, ...) {

  # Overwrite .approved in the ggtern namespace
  .approved <-   c(point     = "point",
                   path      = "path",
                   line      = "line",
                   segment   = "segment",
                   polygon   = "polygon",
                   text      = "text",
                   contour   = "interpolate_tern",
                   density2d = "density_tern",
                   smooth    = "smooth_tern",
                   polygon   = "polygon_tern",
                   rug       = "rug",
                   Tline     = "Tline",
                   Lline     = "Lline",
                   Rline     = "Rline",
                   confidence= "confidence",
                   errorbarT = "errorbart",
                   errorbarL = "errorbarl",
                   errorbarR = "errorbarr",
                   shiftedtext = "shiftedtext")

  utils::assignInNamespace(".approved", .approved, "ggtern")

  GeomShiftedtext$new(mapping = mapping,
                      data = data,
                      stat = stat,
                      position = position,
                      parse = parse, ...)
}

GeomShiftedtext <- proto(ggplot2:::GeomText, {
  objname <- "shiftedtext"
  draw <- function(.,
                   data,
                   scales,
                   coordinates, ...,
                   parse = FALSE,
                   na.rm = FALSE) {
    data <- remove_missing(data,
                           na.rm,
                           c("x", "y", "label"),
                           name = "geom_shiftedtext")

    lab <- data$label
    if (parse) {
      lab <- parse(text = lab)
    }

    with(coord_transform(coordinates, data, scales),
         textGrob(lab,
                  unit(x, "native") + unit(0.375 * size, "mm"),
                  unit(y, "native"),
                  hjust = hjust,
                  vjust = vjust,
                  rot = angle,
                  gp = gpar(col = alpha(colour, alpha),
                            fontfamily = family,
                            fontface = fontface,
                            lineheight = lineheight))
    )
  }
})
