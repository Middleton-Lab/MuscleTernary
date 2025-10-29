#' Define muscle force axis vector
#'
#' Calculate the force axis for a given muscle using two points.
#' Points are assumed to be the centroids of origin and insertion,
#' but can be other points as needed.
#'
#' @param origin_centroid: numeric vector of origin centroid, or equivalent
#' point
#'
#' @param insertion_centroid: numeric vector of insertion centroid, or
#' equivalent point
#'
#' @return Named numeric vector of the axis between the origin and insertion
#' centroids
#'
#' @export
#'
#' @examples
#'
#' central_axis(c(0, 0, 0), c(1, 1, 1))
#'

central_axis <- function(origin_centroid, insertion_centroid) {
  vector <- c(origin_centroid - insertion_centroid)

  namesxyz <- c("x", "y", "z")

  names(vector) <- namesxyz

  make_unit_vector(vector)
}
