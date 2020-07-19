#' Calculate centroid location
#'
#' @param stl String path to stl file
#'
#' @return Numeric vector of centroid location
#' @export
#'
centroid_location <- function(stl) {
  st <- rgl::readSTL(stl, plot = FALSE)
  centroid <- colMeans(st)
  return(centroid)
}
