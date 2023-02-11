#' Estimate PCSA from from a pair of muscle attachment stl meshes.
#'
#' @param stl1 String: Path to stl.
#' @param stl2 String: Path to stl.
#' @param fascicle_length Numeric: Fascicle length
#' @param theta Numeric: Fascicle angle (radians)
#' @param units_adjust Numeric: Multiplier adjustment if units are not in mm.
#' @param stl_area Boolean: Use the area of the stl mesh (default) or use the
#' centroid size.
#'
#' @return numeric: Estimate of PCSA for the muscle defined by the two
#' attachments of `stl1` and `stl2`.
#'
#' @export
#'
pcsa <- function(stl1, stl2,
                 fascicle_length = 1,
                 theta = 0,
                 units_adjust = 1,
                 stl_area = TRUE) {

  message("Assuming theta is measured in radians.")

  if (units_adjust == 1) {
    message("Assuming units are mm.")
  } else {
    message("Scaling by ", units_adjust)
  }

  c1 <- centroid_location(stl1) * units_adjust
  c2 <- centroid_location(stl2) * units_adjust

  if (stl_area) {
    message("Using mesh area for calculation.")
    csize1 <- stl_area(stl1) * units_adjust
    csize2 <- stl_area(stl2) * units_adjust
  } else {
    message("Using centroid size for calculation.")
    csize1 <- centroid_size(stl1) * units_adjust
    csize2 <- centroid_size(stl2) * units_adjust
  }

  Lm <- sqrt((c1[1] - c2[1]) ^ 2 +
               (c1[2] - c2[2]) ^ 2 +
               (c1[3] - c2[3]) ^ 2)

  Vm <- Lm / 3 * (csize1 + csize2 + sqrt(csize1 * csize2))

  return(Vm / (fascicle_length * Lm) * cos(theta))
}
