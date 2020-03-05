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

norm <- function(u) {
  return(sqrt(sum(u ^ 2)))
}

get_euler_angles <- function(A, B) {
  # Calculate the Euler angles for rotating A onto B.
  # The utility is to set up geometry with a known normal (0, 1, 0),
  # and calculate the xyz rotation sequence to match another vector.
  U <- RU(A, B)
  euler_angles <- euler(U)
  return(euler_angles)
}

RU <- function(A, B) {
  # Calculate the rotation matrix required to rotate vector A onto
  # vector B. Both vectors will be normalized to a unit vectors prior to
  # rotation.
  # See: http://math.stackexchange.com/a/897677/6965

  A_norm <- A / norm(A)
  B_norm <- B / norm(B)

  # Calculate the rotation matrix. Note that the order is x, y, x.
  U <- diag(3) +
    ssc(CrossProduct3D(A_norm, B_norm)) +
    ((ssc(CrossProduct3D(A_norm, B_norm)) %*%
         ssc(CrossProduct3D(A_norm, B_norm))) *
       (1 - dot(A_norm, B_norm)) /
       (dot(norm(CrossProduct3D(A_norm, B_norm)), norm(CrossProduct3D(A_norm, B_norm))))
     )
  return(U)
}

ssc <- function(v) {
  # Calculate the skew-symmetric cross-product matrix of v
  # See: See: http://math.stackexchange.com/a/897677/6965
  return(matrix(c(0, -v[3], v[2],
                  v[3], 0, -v[1],
                  -v[2], v[1], 0),
                nrow = 3, byrow = TRUE))
}

euler <- function(U) {
  # Decompose rotation matrix U into Euler angles.  Note that the order
  # is x, y, x.
  # See: http://nghiaho.com/?page_id=846
  xrot <- atan2(U[3, 2], U[3, 3]) * 180 / pi
  yrot <- atan2(-U[3, 1], sqrt(U[3, 2] ^ 2 + U[3, 3] ^ 2)) * 180 / pi
  zrot <- atan2(U[2, 1], U[1, 1]) * 180 / pi

  return(c(xrot, yrot, zrot))
}

vector.cross <- function(a, b) {
  if(length(a)!=3 || length(b)!=3){
    stop("Cross product is only defined for 3D vectors.");
  }
  i1 <- c(2,3,1)
  i2 <- c(3,1,2)
  return (a[i1]*b[i2] - a[i2]*b[i1])
}

# Compute the vector cross product between x and y, and return the components
# indexed by i.
# See: https://stackoverflow.com/a/21736807/168137
CrossProduct3D <- function(x, y, i=1:3) {
  # Project inputs into 3D, since the cross product only makes sense in 3D.
  To3D <- function(x) utils::head(c(x, rep(0, 3)), 3)
  x <- To3D(x)
  y <- To3D(y)

  # Indices should be treated cyclically (i.e., index 4 is "really" index 1, and
  # so on).  Index3D() lets us do that using R's convention of 1-based (rather
  # than 0-based) arrays.
  Index3D <- function(i) (i - 1) %% 3 + 1

  # The i'th component of the cross product is:
  # (x[i + 1] * y[i + 2]) - (x[i + 2] * y[i + 1])
  # as long as we treat the indices cyclically.
  return (x[Index3D(i + 1)] * y[Index3D(i + 2)] -
            x[Index3D(i + 2)] * y[Index3D(i + 1)])
}

CrossProduct2D <- function(x, y) CrossProduct3D(x, y, i=3)
