#' Create a unit vector from a given vector
#'
#' @param x Vector of length 3.
#'
#' @return A normalized unit vector of length 3
#'
#' @export
#'
#' @examples
#' make_unit_vector(c(3, 4, 0))
#' make_unit_vector(c(1, 1, 1))
make_unit_vector <- function(x) {
  if (length(x) != 3){
    stop("Vector of length 3 required.")
  }

  return(x / sqrt((x[1]) ^ 2 + (x[2]) ^ 2 + (x[3]) ^ 2))
}
