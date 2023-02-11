#' Calculate area of an stl mesh
#'
#' @param fname String: path to stl file
#'
#' @return Numeric: Area of the stl mesh
#'
#' @export
#'
stl_area <- function(fname) {
  # Read stl
  st <- read_stl(fname)

  # The structure of the stl files is that the three vertices of a triangle
  # are in sequential rows. So there are nrows() / 3 total vertices.
  #
  # Extract three vertices as separate matrices by indexing 1, 2, 3
  # Then drop the indexing column when separating P, Q, and R.
  S <- cbind(st, rep(1:3, times = dim(st)[1] / 3))
  P <- S[S[, 4] == 1, 1:3]
  Q <- S[S[, 4] == 2, 1:3]
  R <- S[S[, 4] == 3, 1:3]

  # PQ = Q - P
  # PR = R - P
  PQ <- Q - P
  PR <- R - P

  # A = 0.5 * ||PQ × PR||
  # Half the area of the parallelogram defined by PQ and PR
  # Could probably not do this in a loop, but it's fast.
  aa <- numeric(length = nrow(PQ))
  for (ii in 1:nrow(PQ)) {
    xp <- xprod(PQ[ii, ], PR[ii, ])
    aa[ii] <- 0.5 * sqrt(dot(xp, xp))
  }

  return(sum(aa))
}
