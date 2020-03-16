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
#'
write_arrows <- function(muscle, side, x_origin, y_origin, z_origin,
                         x_insertion, y_insertion, z_insertion, force,
                         cylinder_r, cone_r, cone_hr,
                         outfile, rev_arrows) {

  muscle_name <- paste(muscle, side, sep = "_")

  write("/////////////////////////////////////",
        file = outfile, append = TRUE)
  write(paste0("// ", muscle_name),
        file = outfile, append = TRUE)

  if (rev_arrows) {
    insertion_x <- x_insertion
    insertion_y <- y_insertion
    insertion_z <- z_insertion
    origin_x <- x_origin
    origin_y <- y_origin
    origin_z <- z_origin
  } else {
    # Note reversing origin and insertion from coords file to put the arrows
    # on the correct end.
    insertion_x <- x_origin
    insertion_y <- y_origin
    insertion_z <- z_origin
    origin_x <- x_insertion
    origin_y <- y_insertion
    origin_z <- z_insertion
  }

  origin_coords <- paste(origin_x, origin_y, origin_z)
  insertion_coords <- paste(insertion_x, insertion_y, insertion_z)

  # Euler Angles
  origin <- c(origin_x, origin_y, origin_z)
  insertion <- c(insertion_x, insertion_y, insertion_z)
  B <- insertion - origin
  A <- c(0.0, 1.0, 0.0)
  R <- get_euler_angles(A, B)
  rotations <- paste(R, collapse = " ")

  # Create a curve from origin to insertion
  write(paste0('curve -n curve1 -d 1 -p ', origin_coords, ' -p ',
               insertion_coords, ' -k 0 -k 1;'),
        file = outfile, append = TRUE)

  # Create a circle at the origin with a (0, 1, 0) normal
  write(paste0('circle -n circ -ch on -o on -c ', origin_coords,
               ' -nrx 0 -nry 1 -nrz 0 -radius ', cylinder_r, ';'),
        file = outfile, append = TRUE)

  # Apply Euler rotations
  write(paste0('rotate -r -pivot ', origin_coords, ' -xyz ', rotations,
               ' circ;'),
        file = outfile, append = TRUE)

  # Extrude cylinder
  write(paste0('extrude -n ', muscle_name,
               'cyl -et 1 -po 0 circ curve1;'),
        file = outfile, append = TRUE)

  # Make, rotate, and move cone
  write(paste0('cone -n ', muscle_name, 'Cone -po 0 -axis 0 1 0 -r ', cone_r,
               ' -hr ', cone_hr, ';'),
        file = outfile, append = TRUE)
  write(paste0('rotate -r -xyz ', rotations, ' ', muscle_name, 'Cone;'),
        file = outfile, append = TRUE)
  write(paste0('move ', insertion_coords, ' ', muscle_name, 'Cone;'),
        file = outfile, append = TRUE)

  # Clean up
  write('select -r curve1;', file = outfile, append = TRUE)
  write('doDelete;', file = outfile, append = TRUE)
  write('select -r circ;', file = outfile, append = TRUE)
  write('doDelete;', file = outfile, append = TRUE)

  # Apply shader
  shader <- paste0('Color_Presets:', muscle, "SG")
  write(paste0('select -r ', muscle_name, 'Cone ', muscle_name, 'cyl;'),
        file = outfile, append = TRUE)
  write(paste0('hyperShade -assign ', shader, ';'),
        file = outfile, append = TRUE)

  # Reverse surface normals
  write(paste0('reverseSurface -ch on -rpo on -d 3 ', muscle_name, 'cyl;\n\n'),
        file = outfile, append = TRUE)
}
