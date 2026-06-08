.validate_coord_cols <- function(coords) {
  required <- c(
    "muscle", "x_origin", "y_origin", "z_origin",
    "x_insertion", "y_insertion", "z_insertion"
  )
  for (col in required) {
    if (!any(stringr::str_detect(col, names(coords)))) {
      stop(
        'Check column names. At least "muscle", "x_origin",',
        ' "y_origin", "z_origin", "x_insertion",',
        ' "y_insertion", and "z_insertion" must be supplied.'
      )
    }
  }
}

.get_extra_cols <- function(coords) {
  coord_patterns <- c(
    "x_origin", "y_origin", "z_origin",
    "x_insertion", "y_insertion", "z_insertion"
  )
  more_cols <- list()
  for (ii in names(coords)) {
    if (!any(stringr::str_detect(ii, coord_patterns))) {
      more_cols <- append(more_cols, ii)
    }
    more_cols <- as.character(more_cols)
  }
  return(coords |> dplyr::select(dplyr::all_of(more_cols)))
}

.check_col_types <- function(cols_to_keep, grouping) {
  if (!is.null(grouping)) {
    cols_wo_grouping <- cols_to_keep |>
      dplyr::select(-dplyr::one_of(grouping))
    col_classes <- vapply(cols_wo_grouping, class, character(1))
    if ("character" %in% col_classes) {
      stop("'character' class in columns not included for grouping.")
    }
    if ("factor" %in% col_classes) {
      stop("'factor' class in columns not included for grouping.")
    }
  }
}
