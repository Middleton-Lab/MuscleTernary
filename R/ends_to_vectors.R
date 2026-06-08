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
#' @param grouping Character vector of grouping variables for calculating
#' means. Defaults to `NULL`, which will return all the original rows and
#' columns
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
  .validate_coord_cols(coords)

  cols_to_keep <- NULL
  if (ncol(coords) > 6) {
    cols_to_keep <- .get_extra_cols(coords)
    .check_col_types(cols_to_keep, grouping)
  } else if (!is.null(grouping)) { # nocov start
    stop("No additional columns detected, but grouping requested.")
  } # nocov end

  coords_or <- coords |> dplyr::select(dplyr::contains("origin"))
  coords_ins <- coords |>
    dplyr::select(dplyr::contains("insertion"))

  vectors <- as.matrix(coords_or) - as.matrix(coords_ins)
  colnames(vectors) <- c("x", "y", "z")

  unit_vectors <- as.data.frame(
    t(apply(vectors, 1, make_unit_vector))
  )

  if (ncol(coords) > 6) {
    df <- dplyr::bind_cols(cols_to_keep, unit_vectors)
  }

  if (!is.null(grouping)) {
    df <- df |>
      dplyr::group_by_at(grouping) |>
      dplyr::summarise_all(list(~ mean(.)))
    names(df) <- stringr::str_remove(
      names(df), stringr::fixed("_name")
    )
  }

  return(df)
}
