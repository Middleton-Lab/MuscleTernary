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

  # Checks
  if (nrow(x) > 1) {
    stop("x should only have 1 row")
  }

  col_names <- names(x)
  if (!("x_1" %in% col_names) |
      !("y_1" %in% col_names) |
      !("z_1" %in% col_names) |
      !("x_2" %in% col_names) |
      !("y_2" %in% col_names) |
      !("z_2" %in% col_names)) {
    stop("x should have columns x_1, y_1, z_1, x_2, y_2, and z_2")
  }

  # Interpolate to length_out
  interp_pts <- interpolate_xyz(start = x[, c("x_1", "y_1", "z_1")],
                                end = x[, c("x_2", "y_2", "z_2")],
                                length_out = length_out)
  interp_pts <- cbind(x, interp_pts, row.names = NULL)

  return(interp_pts)
}
