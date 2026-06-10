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
      dplyr::select(-dplyr::all_of(grouping))
    col_classes <- vapply(cols_wo_grouping, class, character(1))
    if ("character" %in% col_classes) {
      stop("'character' class in columns not included for grouping.")
    }
    if ("factor" %in% col_classes) {
      stop("'factor' class in columns not included for grouping.")
    }
  }
}

# Shared engine for coords_to_ternary() and ends_to_vectors(). Differences
# their public functions: a per-row `transform` applied to each origin -
# insertion vector, and a `scale` multiplier on the result.
.coords_components <- function(coords, grouping, transform, scale = 1) {
  .validate_coord_cols(coords)

  cols_to_keep <- NULL
  if (ncol(coords) > 6) {
    cols_to_keep <- .get_extra_cols(coords)
    .check_col_types(cols_to_keep, grouping)
  } else if (!is.null(grouping)) { # nocov start
    stop("No additional columns detected, but grouping requested.")
  } # nocov end

  coords_or <- coords |> dplyr::select(dplyr::contains("origin"))
  coords_ins <- coords |> dplyr::select(dplyr::contains("insertion"))

  vectors <- as.matrix(coords_or) - as.matrix(coords_ins)
  colnames(vectors) <- c("x", "y", "z")

  components <- as.data.frame(t(apply(vectors, 1, transform))) * scale

  if (ncol(coords) > 6) {
    df <- dplyr::bind_cols(cols_to_keep, components)
  }

  if (!is.null(grouping)) {
    df <- df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) |>
      dplyr::summarise(dplyr::across(dplyr::everything(), mean))
    names(df) <- stringr::str_remove(
      names(df), stringr::fixed("_name")
    )
  }

  return(df)
}
