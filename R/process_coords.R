#' Process coordinates data.frame
#'
#' @param coords `data.frame` containing coordinates and other
#'   columns to be plotted in ternary space
#' @param grouping Character vector of grouping variables
#' @param forces If `TRUE` (default), tehen check for a column of
#'   force data.
#' @param L_R_means If `TRUE` (default), the means of left and right
#'   muscle are returned.
#'
#' @return \code{data.frame} suitable for plotting with \code{ggtern()}.
#'
#' @export
#'
process_coords <- function(coords, grouping,
                           forces = TRUE,
                           L_R_means = TRUE) {
  require(rlist)

  for (ii in c("muscle", "x_origin", "y_origin", "z_origin",
               "x_insertion", "y_insertion", "z_insertion")) {
    if (!any(str_detect(ii, names(coords)))) {
      stop("Check column names.")
    }
  }

  if (forces) {
    if (!any(str_detect("force", names(coords)))) {
      stop("Need 'force' column.")
    }
  }

  # Get other column names
  if (ncol(coords) > 7) {
    more_cols <- list()
    for (ii in names(coords)) {
      if (!any(str_detect(ii,
                          c("x_origin",
                            "y_origin", "z_origin",
                            "x_insertion", "y_insertion",
                            "z_insertion")))) {
        more_cols <- append(more_cols, ii)
      }
    more_cols <- as.character(more_cols)
    }
    cols_to_keep <- coords %>% dplyr::select(more_cols)
  } else {
    message("No additional columns detected.")
  }

  coords_or <- coords %>% dplyr::select(contains("origin"))
  coords_ins <- coords %>% dplyr::select(contains("insertion"))

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
  if (ncol(coords) > 7) {
    df_to_plot <- dplyr::bind_cols(cols_to_keep, prop_vectors)
  }

  # Make Left_Right column
  df_to_plot$Left_Right <- ifelse(grepl("^L", df_to_plot$muscle), "L", "R")
  df_to_plot$Left_Right <- factor(df_to_plot$Left_Right)

  # Drop L and R from muscle name
  df_to_plot$muscle <- str_remove(df_to_plot$muscle, "L ")
  df_to_plot$muscle <- str_remove(df_to_plot$muscle, "R ")

  # Calculate L-R means if required.
  if (L_R_means) {
    # Make sure that there are even number of rows
    if (nrow(df_to_plot) %% 2 != 0) {
      stop("There probably should be an even number of rows.")
    }

    df_to_plot_mean <- df_to_plot %>%
      dplyr::select(-Left_Right) %>%
      group_by_at(grouping) %>%
        summarise_all(funs(mean))
  } else {
    df_to_plot_mean <- df_to_plot
  }

  return(df_to_plot_mean)
}
