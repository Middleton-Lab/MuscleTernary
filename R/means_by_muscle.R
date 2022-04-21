#' Means by Muscle
#'
#' Aggregate means for each muscle.
#'
#' @param df_no_means \code{data.frame} with two rows for each muscle
#'
#' @return \code{data.frame} with means aggregated for each muscle.
#'
#' @export
#'
means_by_muscle <- function(df_no_means){

  # data.frame to hold factors
  categorical_vars <- data.frame(muscle = df_no_means$muscle)

  # Check for 1 row per muscle
  if (length(unique(df_no_means$muscle)) != nrow(df_no_means) / 2) {
    stop("There do not appear to be two rows per muscle.
  Do not use this function.")
  }

  # variables to drop
  drop_vars <- list()

  # Find categorical variables
  for (i in 2:ncol(df_no_means)){
    variable_name <- names(df_no_means)[i]
    tmp <- df_no_means[, i]
    if (is.factor(tmp)){
      categorical_vars[, variable_name] <- tmp
      drop_vars <- c(drop_vars, variable_name)
    }
  }

  # Drop non-numeric variables
  df_no_means <-
    df_no_means[ , !(names(df_no_means) %in% unlist(drop_vars))]

  message("\nAssuming that any categorical variables alternate by rows.\n")

  # Means by Left_Right
  df_means <- df_no_means |> group_by(muscle) |> summarise_each(funs(mean))

  # Put categorical variables back on. Note: Assumes alternating rows
  categorical_vars <-
    categorical_vars[seq(1, nrow(categorical_vars), by = 2), ]

  df_means <- merge(df_means, categorical_vars)

  return(df_means)
}
