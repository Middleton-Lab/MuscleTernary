#' Calculate the relative proportion of the items of a given vector
#'
#' @param x Vector of length 3
#'
#' @return A normalized unit vector of length 3
#'
#' @export
#'
#' @examples
#' relative_proportion(c(1, 2, 3))
#' relative_proportion(c(3, 4, 0))
relative_proportion <- function(x){

  # Check
  if (length(x) != 3) {
    stop("Vector of length 3 required.")
  }

  return(x^2 / ((x[1])^2 + (x[2])^2 + (x[3])^2))
}
