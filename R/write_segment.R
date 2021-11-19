write_segment <- function(ID,
                          x_origin, y_origin, z_origin,
                          x_insertion, y_insertion, z_insertion,
                          outfile,
                          radius) {

  origin_coords <- paste(x_origin, y_origin, z_origin)
  insertion_coords <- paste(x_insertion, y_insertion, z_insertion)

  # Euler Angles
  origin <- c(x_origin, y_origin, z_origin)
  insertion <- c(x_insertion, y_insertion, z_insertion)

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
               ' -nrx 0 -nry 1 -nrz 0 -radius ', radius, ';'),
        file = outfile, append = TRUE)

  # Apply Euler rotations
  write(paste0('rotate -r -pivot ', origin_coords, ' -xyz ', rotations,
               ' circ;'),
        file = outfile, append = TRUE)

  # Extrude cylinder
  write(paste0('extrude -n ', ID,
               'cyl -et 1 -po 0 circ curve1;'),
        file = outfile, append = TRUE)

  # Clean up
  write('select -r curve1;', file = outfile, append = TRUE)
  write('doDelete;', file = outfile, append = TRUE)
  write('select -r circ;', file = outfile, append = TRUE)
  write('doDelete;', file = outfile, append = TRUE)

  write('\n\n', file = outfile, append = TRUE)
  # Reverse surface normals
  # write(paste0('reverseSurface -ch on -rpo on -d 3 ', ID, 'cyl;\n\n'),
  #       file = outfile, append = TRUE)

}
