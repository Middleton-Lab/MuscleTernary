#' Calculate the relative proportion of the items of a given vector
#'
#' @param x Vector of length 3
#'
#' @return A normalized unit vector of length 3
#'
#' @export
#'
relative_proportion <- function(x) {
  return(x**2 / ((x[1])^2 + (x[2])^2 + (x[3])^2))
}
