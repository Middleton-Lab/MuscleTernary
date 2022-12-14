#' Calculate centroid size for an stl file
#'
#' @param fname String path to stl file
#'
#' @return Numeric Centroid size
#' @export
#'
centroid_size <- function(fname) {
  st <- read_stl(fname)
  centroid <- centroid_location(fname)
  cs <- sqrt(sum((centroid[1] - st[, 1]) ^ 2 +
                   (centroid[2] - st[, 2]) ^ 2 +
                   (centroid[3] - st[, 3]) ^ 2))
  return(cs)
}
