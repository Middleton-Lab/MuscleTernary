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
#' @noRd
#'

mag <- function(vector) {
  if (!is.numeric(vector)) {
    stop("vector should be numeric.") # nocov
  }
  if (length(vector) != 3) {
    stop("vector should be length 3.") # nocov
  }

  namesxyz <- c("x", "y", "z")

  names(vector) <- namesxyz

  return(as.numeric(sqrt(vector["x"]^2 + vector["y"]^2 + vector["z"]^2)))
}
