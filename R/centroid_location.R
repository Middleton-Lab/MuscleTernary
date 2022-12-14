#' Calculate centroid location
#'
#' @param fname String path to stl file
#'
#' @return Numeric vector of centroid location
#' @export
#'
centroid_location <- function(fname) {
  st <- read_stl(fname)
  centroid <- colMeans(st)
  return(centroid)
}
