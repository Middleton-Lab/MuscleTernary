.resolve_arrow_coords <- function(x_origin, y_origin, z_origin,
                                  x_insertion, y_insertion,
                                  z_insertion, rev_arrows) {
  if (rev_arrows) {
    return(list(
      origin_x = x_origin, origin_y = y_origin,
      origin_z = z_origin,
      insertion_x = x_insertion, insertion_y = y_insertion,
      insertion_z = z_insertion
    ))
  }
  return(list(
    origin_x = x_insertion, origin_y = y_insertion,
    origin_z = z_insertion,
    insertion_x = x_origin, insertion_y = y_origin,
    insertion_z = z_origin
  ))
}

.write_mel_arrow_cmds <- function(muscle_name, origin_coords,
                                  insertion_coords, rotations,
                                  cylinder_r, cone_r, cone_hr,
                                  outfile) {
  write(glue::glue('curve -n curve1 -d 1 -p {origin_coords} ',
                   '-p {insertion_coords} -k 0 -k 1;'),
        file = outfile, append = TRUE)
  write(glue::glue('circle -n circ -ch on -o on -c {origin_coords} ',
                   '-nrx 0 -nry 1 -nrz 0 -radius {cylinder_r};'),
        file = outfile, append = TRUE)
  write(glue::glue('rotate -r -pivot {origin_coords} -xyz {rotations} ',
                   'circ;'),
        file = outfile, append = TRUE)
  write(glue::glue('extrude -n {muscle_name}cyl -et 1 -po 0 circ curve1;'),
        file = outfile, append = TRUE)
  write(glue::glue('cone -n {muscle_name}Cone -po 0 -axis 0 1 0 ',
                   '-r {cone_r} -hr {cone_hr};'),
        file = outfile, append = TRUE)
  write(glue::glue('rotate -r -xyz {rotations} {muscle_name}Cone;'),
        file = outfile, append = TRUE)
  write(glue::glue('move {insertion_coords} {muscle_name}Cone;'),
        file = outfile, append = TRUE)
  write('select -r curve1;', file = outfile, append = TRUE)
  write('doDelete;', file = outfile, append = TRUE)
  write('select -r circ;', file = outfile, append = TRUE)
  write('doDelete;', file = outfile, append = TRUE)
  write(glue::glue('select -r {muscle_name}Cone {muscle_name}cyl;'),
        file = outfile, append = TRUE)
  write(glue::glue('sets -e -forceElement ',
                   '{stringr::str_sub(muscle_name, end = -3)}SG;'),
        file = outfile, append = TRUE)
  write(glue::glue('reverseSurface -ch on -rpo on -d 3 ',
                   '{muscle_name}cyl;\n\n', .trim = FALSE),
        file = outfile, append = TRUE)
}

#' Write Maya arrows code
#'
#' @param muscle Muscle
#' @param side Side (L/R)
#' @param x_origin x origin
#' @param y_origin y origin
#' @param z_origin z origin
#' @param x_insertion x insertion
#' @param y_insertion y insertion
#' @param z_insertion z insertion
#' @param force Force (N)
#' @param cylinder_r Cylinder radius
#' @param cone_r Cone radius
#' @param cone_hr Cone height
#' @param outfile String file to write to
#' @param rev_arrows Boolean Reverse arrows or not
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of
#'   writing MEL commands to a file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' write_arrows(
#'   muscle = "mPTd", side = "L",
#'   x_origin = 10, y_origin = 20, z_origin = 30,
#'   x_insertion = 5, y_insertion = 15, z_insertion = 25,
#'   force = 100, cylinder_r = 2, cone_r = 4, cone_hr = 2,
#'   outfile = tempfile(fileext = ".mel"), rev_arrows = TRUE
#' )
#' }
write_arrows <- function(muscle, side, x_origin, y_origin,
                         z_origin, x_insertion, y_insertion,
                         z_insertion, force, cylinder_r, cone_r,
                         cone_hr, outfile, rev_arrows) {
  muscle_name <- paste(muscle, side, sep = "_")

  write("/////////////////////////////////////",
        file = outfile, append = TRUE)
  write(glue::glue("// {muscle_name}"),
        file = outfile, append = TRUE)

  coords <- .resolve_arrow_coords(
    x_origin, y_origin, z_origin,
    x_insertion, y_insertion, z_insertion,
    rev_arrows
  )

  origin_coords <- paste(
    coords$origin_x, coords$origin_y, coords$origin_z
  )
  insertion_coords <- paste(
    coords$insertion_x, coords$insertion_y, coords$insertion_z
  )
  origin <- c(coords$origin_x, coords$origin_y, coords$origin_z)
  insertion <- c(
    coords$insertion_x, coords$insertion_y, coords$insertion_z
  )
  rotations <- paste(
    get_euler_angles(c(0.0, 1.0, 0.0), insertion - origin),
    collapse = " "
  )

  .write_mel_arrow_cmds(
    muscle_name, origin_coords, insertion_coords, rotations,
    cylinder_r, cone_r, cone_hr, outfile
  )

  return(invisible(NULL))
}
