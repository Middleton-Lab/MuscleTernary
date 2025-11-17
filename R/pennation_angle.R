#' Find pennation angle
#'
#' Measures pennation angles by calculating the angle
#' between the central axis and each fiber endpoint vector
#'
#' @param vector_table: 'data.frame' or 'tibble 'with unit vectors produced by
#' 'ends_to_vectors()'
#'
#' @param central axis: Named numeric vector such as that produced by
#' 'central_axis()'
#'
#' @return Xfiber dataframe/tibble with newly appended "PennationAngle" column
#'
#' @export
#'

pennation_angle <- function(vector_table, central_axis) {
  PennationAngle <- c()

  for (ii in 0:(length(vector_table$track_num) - 1)) {
    fiber_vector <- subset(vector_table, track_num == ii, select = c(x, y, z))
    fiber_vector <- as.numeric(fiber_vector)
    angle <- theta(fiber_vector, central_axis)

    angle <- angle * (180 / pi) #Conversion to degrees from radians

    if (angle > 90) {
      #for reversing angles that are obtuse

      angle <- (180 - angle)
    }
    PennationAngle <- append(PennationAngle, angle)
  }

  #Adding new pennation angle column to original data frame

  PennationAngle <- unlist(PennationAngle, use.names = FALSE)
  vector_table$PennationAngle <- PennationAngle

  return(vector_table)
}
