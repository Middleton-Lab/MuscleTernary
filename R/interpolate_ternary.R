#' Interpolate between two point in a coordinates object.
#'
#' @param x Row of a coordinates file. Should contain columns `x_1`,
#'   `y_1`, `z_1`, `x_2`, `y_2`, `z_2`, for example in the output of
#'   `merge_coords()`.
#' @param length_out Total length of output `data.frame`.
#'
#' @return `data.frame` with interpolated points added. All other
#'   variables are copied to fill.
#'
#' @export
#'
interpolate_ternary <- function(x, length_out = 200){
  interp_pts <- interpolate_xyz(start = x[, c("x_1", "y_1", "z_1")],
                                end = x[, c("x_2", "y_2", "z_2")],
                                length_out = length_out)
  interp_pts <- cbind(x, interp_pts, row.names = NULL)
  return(interp_pts)
}
