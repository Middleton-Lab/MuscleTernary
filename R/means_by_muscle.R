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

  Muscle_Group <- df_no_means$Muscle_Group

  message("Assuming that any categorical variables alternate by rows.")

  # Means by Left_Right
  df_means <- df_no_means %>% group_by(muscle) %>% summarise_each(funs(mean))

  # Put Muscle_Group back on. Note: Assumes alternating rows
  df_means$Muscle_Group <- Muscle_Group[seq(1, nrow(df_no_means), by = 2)]

  return(df_means)
}