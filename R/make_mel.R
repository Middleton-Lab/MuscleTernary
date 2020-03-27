#' Write mel file
#'
#' @param stl string Location of the stl file. Assumed to present in the
#' working directory
#' @param data data.frame Object with the data
#' @param shader_file string Either "default" to use the standard palette,
#' or the path to a csv file with colors for each muscle.
#' @param outfile string Name of file to write to. Defaults to the base name
#' of the stl file.
#' @param scale_radius boolean (default \code{TRUE}) Should the radius be
#' scaled to the maximum value automatically based on the stl centroid size?
#' @param max_radius numeric Maximum radius value. Ignored if
#' \code{scale_radius} is \code{TRUE}.
#' @param rev_arrows boolean (default \code{TRUE}) Should the arrowheads be
#' reversed (you probably want \code{TRUE})
#'
#' @export
#'
make_mel <- function(stl,
                     data,
                     shader_file = "default",
                     outfile = NULL,
                     scale_radius = TRUE,
                     max_radius = 8,
                     rev_arrows = TRUE) {

  # Check column names
  col_needed <- c("x_origin", "y_origin", "z_origin",
                  "x_insertion", "y_insertion", "z_insertion", "muscle",
                  "force", "side")
  col_names <- names(data)
  if (length(intersect(col_needed, col_names)) != 9) {
    stop(strwrap('Columns must include "x_origin", "y_origin", "z_origin",
        "x_insertion", "y_insertion", "z_insertion", "muscle", "side",
                 "force"'))
  }

  # Extract file info
  file_prefix <- stringr::str_sub(stl, start = 1L, end = -5L)
  stl_path <- file.path(getwd(), stl)
  if(is.null(outfile)) outfile <- paste0(file_prefix, ".mel")

  message("Assuming that the stl file is found at ", stl_path, "\n")
  message("Writing .mel file to ", outfile, "\n")
  if (rev_arrows) {
    message(strwrap("Reversing the arrowheads. If they point the wrong
                    direction, change rev_arrows to FALSE.",
                    prefix = " ", initial = ""), "\n")
  } else {
    message(strwrap("Not reversing the arrowheads. If they point the wrong
                    direction, change rev_arrows to TRUE.",
                    prefix = " ", initial = ""), "\n")
  }

  # Set up radii for cylinder and cone
  if (scale_radius) {
    # Read stl file and get centroid size
    centroid <- centroid_size(stl)
    max_radius <- 1.126e-01 + 6.488e-05 * centroid

    # Normalize to maximum force value
    data <- data %>%
      mutate(cylinder_r = force / max(force) * max_radius)
  } else{
    data <- data %>%
      mutate(cylinder_r = max_radius / 2)
  }

  message("Note: the ratio of max to min forces is ",
          round(max(data$force) / min(data$force), 3), "\n")
  message(strwrap(paste0("The maximum cylinder width is ",
                         round(max(data$cylinder_r), 3),
                         ". Adjust this based on the size of your model
                         overall.")))

  data$cone_r <- data$cylinder_r * 2
  data$cone_hr <- 2  # cone_r / 2

  # Write header info
  write(paste0("// File: ", outfile), file = outfile)
  write(paste0("// Generated: ",
               format(Sys.time(), "%a %b %d %H:%M:%S %Y")),
        file = outfile, append = TRUE)
  write(paste0("// Note: the ratio of max to min forces is ",
               round(max(data$force) / min(data$force), 3)),
        file = outfile, append = TRUE)

  write('', file = outfile, append = TRUE)

  # Generate shader
  if (shader_file == "default") {
    shader <- readr::read_csv(system.file("extdata",
                                   "muscle_colors.csv",
                                   package = "MuscleTernary"))
  } else {
    shader <- readr::read_csv(shader)
  }
  generate_shader(shader, outfile)

  # Import model. Note need full path to stl.
  write('\n// Import stl model', file = outfile, append = TRUE)
  write(paste0('file -import -type "STLImport" -ignoreVersion -ra true ',
               '-mergeNamespacesOnClash false -namespace "',
               file_prefix, '" -pr "', stl_path, '";'),
        file = outfile, append = TRUE)
  write(paste0('select -r ', file_prefix, ';'),
        file = outfile, append = TRUE)
  write('sets -e -forceElement BoneSG;',
        file = outfile, append = TRUE)
  write('', file = outfile, append = TRUE)

  # Iterate through rows of data and write code for making arrows
  nul <- pmap(data, write_arrows,
              outfile = outfile,
              rev_arrows = rev_arrows)
}
