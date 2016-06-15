#' Merge coordinates files
#'
#' `merge_coords()` takes two coordinates files and merges them into
#' one file. Variable names are post-pended with '_1' and '_2'.
#'
#' @param M1 `data.frame` with coordinate data.
#' @param M2 `data.frame` with coordinate data.
#'
#' @return `data.frame` with merged coordinate data.
#'
#' @export
#'
#' @examples
#'
#' AL_008 <- read_coords(system.file("extdata",
#'                                   "AL_008_coords.xlsx",
#'                                   package = "MuscleTernary"),
#'                       system.file("extdata",
#'                                   "AL_008_forces.xlsx",
#'                                   package = "MuscleTernary"),
#'                       L_R_means = TRUE)
#' AL_031 <- read_coords(system.file("extdata",
#'                                   "AL_031_coords.xlsx",
#'                                   package = "MuscleTernary"),
#'                       system.file("extdata",
#'                                   "AL_031_forces.xlsx",
#'                                   package = "MuscleTernary"),
#'                       L_R_means = TRUE)
#'
#' M <- merge_coords(AL_031, AL_008)
#' M
#'
merge_coords <- function(M1, M2) {

  # Checks
  if (!all.equal(names(M1), names(M2))) {
    stop("Column names of M1 and M2 do not match.")
  }

  names(M1) <- paste0(names(M1), "_1")
  names(M2) <- paste0(names(M2), "_2")
  M <- cbind(M1, M2)

  return(M)
}
