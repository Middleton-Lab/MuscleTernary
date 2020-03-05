#' Dot product
#'
#' @param u Numeric vector
#' @param v Numeric vector
#'
#' @return Numeric dot product of `u` and `v`.
#'
dot <- function(u, v) {
  return(sum(u * v))
}


#' Normalize a vector (L2 norm)
#'
#' @param u Vector
#'
#' @return L2 norm of `u`.
#'
norm <- function(u) {
  return(sqrt(sum(u ^ 2)))
}


#' Calculate the Euler angles for rotating A onto B.
#'
#' The utility is to set up geometry with a known normal (0, 1, 0),
#' and calculate the xyz rotation sequence to match another vector.
#'
#' @param A Vector (usually the unit vector)
#' @param B Vector
#'
#' @return Euler angles between A and B
#'
#' @export
#'
get_euler_angles <- function(A, B) {
  U <- RU(A, B)
  euler_angles <- euler(U)
  return(euler_angles)
}


#' Calculate rotation matrix
#'
#' Calculate the rotation matrix required to rotate vector A onto
#' vector B. Both vectors will be normalized to a unit vectors prior to
#' rotation.
#' See: http://math.stackexchange.com/a/897677/6965
#'
#' @param A Vector (usually the unit vector)
#' @param B Vector
#'
#' @return U rotation matrix between A and B
#'
RU <- function(A, B) {
  A_norm <- A / norm(A)
  B_norm <- B / norm(B)

  # Calculate the rotation matrix. Note that the order is x, y, x.
  U <- diag(3) +
    ssc(CrossProduct3D(A_norm, B_norm)) +
    ((ssc(CrossProduct3D(A_norm, B_norm)) %*%
         ssc(CrossProduct3D(A_norm, B_norm))) *
       (1 - dot(A_norm, B_norm)) /
       (dot(norm(CrossProduct3D(A_norm, B_norm)),
            norm(CrossProduct3D(A_norm, B_norm))))
     )
  return(U)
}


#' Skew-symmetric cross-product
#'
#' Calculate the skew-symmetric cross-product matrix of v
#' See: See: http://math.stackexchange.com/a/897677/6965
#'
#' @param v Matrix
#'
#' @return Skew-symmetric cross-product of `v`
#'
ssc <- function(v) {
  return(matrix(c(0, -v[3], v[2],
                  v[3], 0, -v[1],
                  -v[2], v[1], 0),
                nrow = 3, byrow = TRUE))
}


#' Decompose rotation matrix U into Euler angles
#'
#' Note that the order
#' is x, y, x.
#' See: http://nghiaho.com/?page_id=846
#'
#' @param U Rotation matrix
#'
#' @return Euler angles
#'
#' @export
#'
euler <- function(U) {
  xrot <- atan2(U[3, 2], U[3, 3]) * 180 / pi
  yrot <- atan2(-U[3, 1], sqrt(U[3, 2] ^ 2 + U[3, 3] ^ 2)) * 180 / pi
  zrot <- atan2(U[2, 1], U[1, 1]) * 180 / pi

  return(c(xrot, yrot, zrot))
}


#' Compute vector cross product
#'
#' Compute the vector cross product between x and y, and return the
#' components indexed by i.
#' See: https://stackoverflow.com/a/21736807/168137
#'
#' @param x Vector or matrix
#' @param y Vector or matrix
#' @param i Indices
#'
#' @return Cross product matrix
#'
CrossProduct3D <- function(x, y, i=1:3) {
  # Project inputs into 3D, since the cross product only makes sense in 3D.
  To3D <- function(x) utils::head(c(x, rep(0, 3)), 3)
  x <- To3D(x)
  y <- To3D(y)

  # Indices should be treated cyclically (i.e., index 4 is "really" index 1,
  # and so on).  Index3D() lets us do that using R's convention of 1-based
  # (rather than 0-based) arrays.
  Index3D <- function(i) (i - 1) %% 3 + 1

  # The i'th component of the cross product is:
  # (x[i + 1] * y[i + 2]) - (x[i + 2] * y[i + 1])
  # as long as we treat the indices cyclically.
  return (x[Index3D(i + 1)] * y[Index3D(i + 2)] -
            x[Index3D(i + 2)] * y[Index3D(i + 1)])
}
