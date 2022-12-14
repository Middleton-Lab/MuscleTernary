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
coords_to_ternary <- function(coords, grouping = NULL) {

  for (ii in c("muscle", "x_origin", "y_origin", "z_origin",
               "x_insertion", "y_insertion", "z_insertion")) {
    if (!any(str_detect(ii, names(coords)))) {
      stop('Check column names. At least "muscle", "x_origin",
            "y_origin", "z_origin", "x_insertion", "y_insertion",
            and "z_insertion" must be supplied.')
    }
  }

  # Get other column names
  if (ncol(coords) > 6) {
    more_cols <- list()
    for (ii in names(coords)) {
      if (!any(stringr::str_detect(ii,
                          c("x_origin",
                            "y_origin", "z_origin",
                            "x_insertion", "y_insertion",
                            "z_insertion")))) {
        more_cols <- append(more_cols, ii)
      }
      more_cols <- as.character(more_cols)
    }
    cols_to_keep <- coords |> dplyr::select(all_of(more_cols))
  } else if (!is.null(grouping)) {
    stop("No additional columns detected, but grouping requested.")
  }

  # Check that additional, non-grouping columns aren't character
  if (ncol(coords) > 6) {
    if (!is.null(grouping)) {
      cols_wo_grouping <- cols_to_keep |>
        dplyr::select(-one_of(grouping))
      col_classes <- sapply(cols_wo_grouping, class)
      if ("character" %in% col_classes) {
        stop("'character' class in columns not included for grouping.")
      }
      if ("factor" %in% col_classes) {
        stop("'factor' class in columns not included for grouping.")
      }
    }
  }

  coords_or <- coords |> dplyr::select(contains("origin"))
  coords_ins <- coords |> dplyr::select(contains("insertion"))

  # Calculate vector from origin to insertion.
  vectors <- as.matrix(coords_or) - as.matrix(coords_ins)
  colnames(vectors) <- c("x", "y", "z")

  # Pass rows sequentially to make_unit_vector and relative proportion
  unit_vectors <- t(apply(vectors, 1, make_unit_vector))
  prop_vectors <- t(apply(vectors, 1, relative_proportion))

  # Need a data.frame for ggplot.
  prop_vectors <- as.data.frame(prop_vectors)

  # Scale to percentage
  prop_vectors <- prop_vectors * 100

  # Reattach cols_to_keep
  if (ncol(coords) > 6) {
    df <- dplyr::bind_cols(cols_to_keep, prop_vectors)
  }

  # Means based on grouping variables
  if (!is.null(grouping)) {
    df <- df |>
      group_by_at(grouping) |>
      summarise_all(list(~mean(.)))
    names(df) <- stringr::str_remove(names(df), "_name")
  }

  return(df)
}
