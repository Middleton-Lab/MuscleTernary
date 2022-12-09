#' Estimate PCSA from from a pair of muscle attachment stls.
#'
#' @param stl1 string: Path to stl.
#' @param stl2 string: Path to stl.
#' @param fascicle_length numeric: Fascicle length
#' @param theta numeric: Fascicle angle (radians)
#' @param units_adjust numeric: Multiplier adjustment if units are not in mm.
#'
#' @return numeric: Estimate of PCSA for the muscle defined by the two
#' attachments of `stl1` and `stl2`.
#'
#' @export
#'
PCSA <- function(stl1, stl2,
                 fascicle_length = 1,
                 theta = 0,
                 units_adjust = 1) {

  if (units_adjust == 1) {
    message("Assuming units are mm.")
  } else {
    message("Scaling by ", units_adjust)
  }

  c1 <- centroid_location(stl1) * units_adjust
  c2 <- centroid_location(stl2) * units_adjust

  csize1 <- centroid_size(stl1) * units_adjust
  csize2 <- centroid_size(stl2) * units_adjust

  Lm <- sqrt((c1[1] - c2[1]) ^ 2 + (c1[2] - c2[2]) ^ 2 + (c1[3] - c2[3]) ^ 2)

  Vm <- Lm / 3 * (csize1 + csize2 + sqrt(csize1 * csize2))

  return(Vm / (fascicle_length * Lm) * cos(theta))
}
