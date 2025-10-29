#' Calculate vector magnitude
#'
#' Calculates the magnitude of a given vector
#'
#' @param vector Vector whose magnitude you want to measure
#'
#' @return Numeric: Magnitude of the vector (x, y, z).
#'
#' @keywords internal
#'

mag <- function(vector) {
  if (!is.numeric(vector)) {
    stop("vector should be numeric.")
  }
  if (length(vector) != 3) {
    stop("vector should be length 3.")
  }

  namesxyz <- c("x", "y", "z")

  names(vector) <- namesxyz

  return(as.numeric(sqrt(vector["x"]^2 + vector["y"]^2 + vector["z"]^2)))
}
