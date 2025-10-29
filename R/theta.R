#' Compute 3D vector cross product
#'
#' Calculates the angle between two vectors
#'
#' @param x Vector
#'
#' @param y Vector
#'
#' @return Angle 'theta' between the two input vectors in radians
#'
#' @keywords internal
#'

theta <- function(vector1, vector2) {
  if (!is.numeric(vector1)) {
    stop("vector should be numeric.")
  }
  if (length(vector1) != 3) {
    stop("vector should be length 3.")
  }

  if (!is.numeric(vector2)) {
    stop("vector should be numeric.")
  }
  if (length(vector2) != 3) {
    stop("vector should be length 3.")
  }

  namesxyz <- c("x", "y", "z")
  names(vector1) <- namesxyz
  names(vector2) <- namesxyz

  return(acos((dot(vector1, vector2)) / (mag(vector1) * mag(vector2))))
}
