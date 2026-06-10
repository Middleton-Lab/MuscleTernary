#'
#' Function turning xfiber endpoint vectors into unit vectors
#' Should be used following 'find_track_ends'
#'
#'
#' @param coords `data.frame` or `tibble` containing coordinates and other
#'   columns to be converted into unit vectors. Required columns are at least
#'   "muscle", "x_origin", "y_origin", "z_origin", "x_insertion",
#'   "y_insertion", and "z_insertion".
#'
#' @inheritParams coords_to_ternary
#'
#' @return 'data.frame' with x, y, and z components of unit vectors of each
#' fiber track
#'
#' @export
#'
#' @examples
#' D <- read_xfiber_xml(system.file("extdata",
#'                      "AV069_SC.xml",
#'                      package = "MuscleTernary")) |>
#'   dplyr::mutate(muscle = "SC")
#' ends <- find_track_ends(D)
#' ends_to_vectors(ends)
#'
#'
ends_to_vectors <- function(coords, grouping = NULL) {
  return(.coords_components(coords, grouping,
                            transform = make_unit_vector))
}
