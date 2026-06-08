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

  prop_vectors <- as.data.frame(
    t(apply(vectors, 1, relative_proportion))
  ) * 100

  if (ncol(coords) > 6) {
    df <- dplyr::bind_cols(cols_to_keep, prop_vectors)
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
