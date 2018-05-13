#' Read and process coordinates file
#'
#' @param coords_file String with path to coordinates file.
#' @param force_file (Optional) String with path to force file.
#' @param L_R_means If `TRUE` (default), the means of left and right
#'   muscle are returned.
#'
#' @return \code{data.frame} suitable for plotting with \code{ggtern()}.
#'
#' @export
#'
#' @examples
#'
#' read_coords(system.file("extdata",
#'                         "AL_008_coords.xlsx",
#'                         package = "MuscleTernary"),
#'             system.file("extdata",
#'                         "AL_008_forces.xlsx",
#'                         package = "MuscleTernary"),
#'                         L_R_means = TRUE)
#'
#' read_coords(system.file("extdata",
#'                         "AL_008_coords.xlsx",
#'                         package = "MuscleTernary"),
#'             system.file("extdata",
#'                         "AL_008_forces.xlsx",
#'                         package = "MuscleTernary"),
#'                         L_R_means = FALSE)
#'
read_coords <- function(coords_file,
                        force_file = NULL,
                        L_R_means = TRUE){
  # Read coordinate data
  M <- read_excel(coords_file)

  col_names <- c("muscle", "x_origin", "y_origin", "z_origin",
                 "x_insertion", "y_insertion", "z_insertion")

  # Check column names
  if (!all.equal(names(M), col_names)) {
    stop('Column names need fixing.\n
         Should be "muscle", "x_origin", "y_origin", "z_origin",\n
         "x_insertion", "y_insertion", "z_insertion"')
  }

  # Read force data
  if (!is.null(force_file)) {
    M_force <- read_excel(force_file)
    M_force <- M_force[complete.cases(M_force), ]

    for (i in 2:ncol(M_force)) {
      variable_name <- names(M_force)[i]

      # See: http://stackoverflow.com/q/21618423/168137
      tmp <- M_force %>% select(matches(variable_name)) %>%
        collect %>% .[[variable_name]]
      if (is.character(tmp)) {
        M_force[, variable_name] <- factor(tmp)
      }
    }
  }

  # Split off origin and insertion columns
  coords_or <- M[, c("x_origin", "y_origin", "z_origin")]
  coords_ins <- M[, c("x_insertion", "y_insertion", "z_insertion")]

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

  # Put Muscle column back onto prop_vector
  prop_vectors$muscle <- M$muscle

  # Merge if M_force exists)
  if (exists("M_force")) {
    df_to_plot <- merge(prop_vectors, M_force)
  } else {
    message("Force file not provided.")
    df_to_plot <- prop_vectors
  }

  # Make Left_Right column
  df_to_plot$Left_Right <- ifelse(grepl("^L", df_to_plot$muscle), "L", "R")
  df_to_plot$Left_Right <- factor(df_to_plot$Left_Right)

  # Drop L and R from muscle name
  df_to_plot$muscle <- gsub("L ", "", df_to_plot$muscle)
  df_to_plot$muscle <- gsub("R ", "", df_to_plot$muscle)

  # Sort by muscle
  df_to_plot <- df_to_plot %>% arrange(muscle)


  # Calculate L-R means if required.
  if (L_R_means) {
    # Make sure that there are even number of rows
    if (nrow(df_to_plot) %% 2 != 0) {
      stop("There probably should be an even number of rows.")
    }

    df_to_plot_mean <- df_to_plot %>% select(muscle, x, y, z, force) %>%
      group_by(muscle) %>% summarise_each(funs(mean))
    tmp_rows_to_merge <- df_to_plot %>%
      select(-c(muscle, x, y, z, force, Left_Right)) %>%
      slice(seq(1, nrow(df_to_plot), 2))
    df_to_plot_mean <- cbind(df_to_plot_mean, tmp_rows_to_merge)
  } else {
    df_to_plot_mean <- df_to_plot
  }

  return(df_to_plot_mean)
}
