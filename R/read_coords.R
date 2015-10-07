#' Read and process coordinates file
#'
#' @param coords_file String with path to coordinates file.
#' @param force_file (Optional) String with path to force file.
#'
#' @return \code{data.frame} suitable for plotting with \code{ggtern()}.
#' @export
#'
read_coords <- function(coords_file,
                        force_file = NULL){
  # Read coordinate data
  M <- read_excel(coords_file)

  col_names <- c("muscle", "x_origin", "y_origin", "z_origin",
                 "x_insertion", "y_insertion", "z_insertion")

  # Check column names
  if (!all.equal(names(M), col_names)){
    stop('Column names need fixing.\n
         Should be "muscle", "x_origin", "y_origin", "z_origin",\n
         "x_insertion", "y_insertion", "z_insertion"')
  }

  # Read force data
  if (!is.null(force_file)){
    M_extras <- read_excel(force_file)
    M_extras <- M_extras[complete.cases(M_extras), ]

    for (i in 2:ncol(M_extras)){
      variable_name <- names(M_extras)[i]

      # See: http://stackoverflow.com/q/21618423/168137
      tmp <- M_extras %>% select(matches(variable_name)) %>%
        collect %>% .[[variable_name]]
      if (is.character(tmp)){
        M_extras[, variable_name] <- factor(tmp)
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

  # Merge if M_extras exists)
  if (exists("M_extras")){
    df_to_plot <- merge(prop_vectors, M_extras)
  } else {
    df_to_plot <- prop_vectors
  }

  # Make Left_Right column
  df_to_plot$Left_Right <- ifelse(grepl("^L", df_to_plot$muscle), "L", "R")
  df_to_plot$Left_Right <- factor(df_to_plot$Left_Right)

  # Drop L and R
  df_to_plot$muscle <- gsub("L ", "", df_to_plot$muscle)
  df_to_plot$muscle <- gsub("R ", "", df_to_plot$muscle)

  # Sort by muscle
  df_to_plot <- df_to_plot %>% arrange(muscle)

  # Make sure that there are even number of rows
  if (nrow(df_to_plot) %% 2 != 0){
    stop("There probably should be an even number of rows.")
  }

  return(df_to_plot)
}
