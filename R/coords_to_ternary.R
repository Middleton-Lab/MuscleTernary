#' Process coordinates data.frame
#'
#' @param coords `data.frame` or `tibble` containing coordinates and other
#'   columns to be plotted in ternary space. Required columns are at least
#'   "muscle", "x_origin", "y_origin", "z_origin", "x_insertion",
#'   "y_insertion", and "z_insertion"
#' @param grouping Character vector of grouping variables for calculating
#' means. Defaults to `NULL`, which will return all the original rows and
#' columns
#'
#' @return \code{data.frame} suitable for plotting with \code{ggtern()}.
#'
#' @export
#'
#' @examples
#' coords <- data.frame(
#'   muscle = "mPTd",
#'   x_origin = 10, y_origin = 20, z_origin = 30,
#'   x_insertion = 5, y_insertion = 15, z_insertion = 25
#' )
#' coords_to_ternary(coords)
coords_to_ternary <- function(coords, grouping = NULL) {
  return(.coords_components(coords, grouping,
                            transform = relative_proportion,
                            scale = 100))
}
