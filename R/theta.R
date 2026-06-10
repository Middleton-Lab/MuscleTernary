#' Compute 3D vector cross product
#'
#' Calculates the angle between two vectors
#'
#' @param vector1 numeric vector of length 3
#'
#' @param vector2 numeric vector of length 3
#'
#' @return Angle 'theta' between the two input vectors in radians
#'
#' @keywords internal
#'
#' @noRd
#'

theta <- function(vector1, vector2) {
  if (!is.numeric(vector1)) {
    stop("vector should be numeric.") # nocov
  }
  if (length(vector1) != 3) {
    stop("vector should be length 3.") # nocov
  }

  if (!is.numeric(vector2)) {
    stop("vector should be numeric.") # nocov
  }
  if (length(vector2) != 3) {
    stop("vector should be length 3.") # nocov
  }

  namesxyz <- c("x", "y", "z")
  names(vector1) <- namesxyz
  names(vector2) <- namesxyz

  return(acos((dot(vector1, vector2)) / (mag(vector1) * mag(vector2))))
}
