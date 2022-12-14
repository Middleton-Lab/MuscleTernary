#' Import STL mesh
#'
#' This is a wrapper of `rgl::readSTL()`, which returns `NA` if the read
#' fails (which it does for both ASCII STLs and binary STLs that use big
#' endian encoding).
#'
#' @param fname string path to stl file
#'
#' @return numeric matrix with (x, y, z) coordinates of vertices on successful
#' read. `NA` on any warning.
#'
#' @export
#'
#' @examples
#'
#' read_stl(system.file("extdata",
#'                      "L_mAMEM_or.stl",
#'                      package = "MuscleTernary"))
#'
read_stl <- function(fname) {
  tryCatch(
    {
      suppressWarnings(st <- rgl::readSTL(fname, plot = FALSE))
      return(st)
    },
    error = function(cond) {
      message("stl file should be saved as binary with little endian encoding")
      message("Returning NA")
      return(NA)
    })
}
