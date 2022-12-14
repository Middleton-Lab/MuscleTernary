#' Interpolate (x, y, z) coordinates
#'
#' Given a start and end point for a pair of xyz coordinate, return
#' the linearly interpolated data.frame between them. This functional
#' also adds a `.frame` column that allows animation.
#'
#' @param start Vector of xyz coordinate for start position.
#' @param end Vector of xyz coordinate for end position.
#' @param length_out Total length of output `data.frame`.
#'
#' @return `data.frame` interpolated between `start` and `end` with
#'   column for frame number.
#'
#' @keywords internal
#'
interpolate_xyz <- function(start, end, length_out){

  # Checks
  if (length(start) != 3) {
    stop("start does not have length of 3")
  }
  if (length(end) != 3) {
    stop("end does not have length of 3")
  }

  out <- matrix(NA, ncol = 3, nrow = length_out)
  out[, 1] <- seq(start[1, 1], end[1, 1], length.out = length_out)
  out[, 2] <- seq(start[1, 2], end[1, 2], length.out = length_out)
  out[, 3] <- seq(start[1, 3], end[1, 3], length.out = length_out)
  out <- as.data.frame(out)
  names(out) <- c("x", "y", "z")
  out$.frame <- 1:length_out

  return(out)
}
