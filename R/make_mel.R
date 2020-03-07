#' Write mel file
#'
#' @param stl String with the stl file. Assumed to present in the working
#' directory
#' @param data Data.frame with the data
#' @param outfile String Name of file to write to. Defaults to the base name
#' of the stl file.
#' @param scale_radius Boolean (default TRUE) Should the radius be scaled to
#' the maximum value
#' @param max_radius Numeric Maximum radius value
#' @param rev_arrows Boolean (default TRUE) Should the arrows be reversed
#' (you probably want TRUE)
#'
#' @export
#'
make_mel <- function(stl,
                     data,
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
    # Normalize to maximum force value
    data <- data %>%
      mutate(cylinder_r = force / max(force) * max_radius)
  } else{
    data <- data %>%
      mutate(cylinder_r = max_radius / 2)
  }

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
  message("Note: the ratio of max to min forces is ",
          round(max(data$force) / min(data$force), 3), "\n")
  message(strwrap(paste0("The maximum cylinder width is ",
                         round(max(data$cylinder_r, 3)),
                         ". Adjust this based on the size of your model
                         overall.")))

  write('', file = outfile, append = TRUE)

  # Import shader information
  write('// Import color shader presets', file = outfile, append = TRUE)
  write(paste0('file -import -type "mayaBinary" -ignoreVersion -ra true',
               '-mergeNamespacesOnClash false -namespace "Color_Presets" ',
               '-options "v=0;"  -pr "Color_Presets.mb";'),
        file = outfile, append = TRUE)

  # Import model. Note need full path to stl.
  write('// Import stl model', file = outfile, append = TRUE)
  write(paste0('file -import -type "STLImport" -ignoreVersion -ra true ',
               '-mergeNamespacesOnClash false -namespace "',
               file_prefix, '" -pr "', stl_path, '";'),
        file = outfile, append = TRUE)
  write(paste0('select -r ', file_prefix, ';'),
        file = outfile, append = TRUE)
  write('hyperShade -assign Color_Presets:Bone;',
        file = outfile, append = TRUE)
  write(paste0('hide ', file_prefix, ';\n'),
        file = outfile, append = TRUE)

  # Iterate through rows of data and write code for making arrows
  nul <- pmap(data, write_arrows,
              outfile = outfile,
              rev_arrows = rev_arrows)

  # Unhide stl_model
  write('// Unhide model;', file = outfile, append = TRUE)
  write(paste0('showHidden ', file_prefix, ';'),
        file = outfile, append = TRUE)
}
