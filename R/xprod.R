#' Cross-product of two 3d vectors
#'
#' This function calculates the cross-product of two 3d (3 element) vectors.
#' Note that this is different from `base::crossprod()`.
#'
#' @param a numeric vector of length 3
#' @param b numeric vector of length 3
#'
#' @return numeric vector of length 3
#'
#' @export
#'
#' @examples
#' xprod(c(0, 1, 2), c(1, 1, 1))
#'
xprod <- function(a, b) {
  if (length(a) & length(b) != 3) {
    stop("a and b must be of length 3.")
  }
  c(a[2] * b[3] - a[3] * b[2],
    a[3] * b[1] - a[1] * b[3],
    a[1] * b[2] - a[2] * b[1])
}
