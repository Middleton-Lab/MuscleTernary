#' Calculate centroid location
#'
#' @param fname String: Path to stl file
#'
#' @return Numeric vector of centroid location
#' @export
#'
#' @examples
#' f <- system.file("extdata", "L_mPTd_Or.stl",
#'                  package = "MuscleTernary")
#' centroid_location(f)
centroid_location <- function(fname) {
  st <- read_stl(fname)
  centroid <- colMeans(st)
  return(centroid)
}
