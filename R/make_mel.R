#' Title
#'
#' @param stl
#' @param data
#' @param scale_radius
#' @param max_radius
#' @param rev_arrows
#'
#' @return
#' @export
#'
#' @examples
make_mel <- function(stl,
                     data,
                     scale_radius = TRUE,
                     max_radius = 8,
                     rev_arrows = TRUE) {
  file_prefix <- str_sub(stl, start = 1L, end = -5L)
  stl_path <- file.path(getwd(), stl)
  outfile <- paste0(file_prefix, ".mel")

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
  write(paste0('// File: ', outfile), file = outfile)
  write(paste0('// Generated: ',
               format(Sys.time(), "%a %b %d %H:%M:%S %Y")),
        file = outfile, append = TRUE)
  write(paste0('// Note: the ratio of max to min forces is ',
               round(max(M$force) / min(M$force), 3)),
        file = outfile, append = TRUE)
  write('\n', file = outfile, append = TRUE)

  # Import shader information
  write('// Import color shader presets', file = outfile, append = TRUE)
  write('file -import -type "mayaBinary" -ignoreVersion -ra true -mergeNamespacesOnClash false -namespace "Color_Presets" -options "v=0;"  -pr "Color_Presets.mb";',
        file = outfile, append = TRUE)

  # Import model. Note need full path to stl.
  write('// Import stl model', file = outfile, append = TRUE)
  write(paste0('file -import -type "STLImport" -ignoreVersion -ra true -mergeNamespacesOnClash false -namespace "',
               file_prefix, '" -pr "', stl_path, '";'),
        file = outfile, append = TRUE)
  write(paste0('select -r ', file_prefix, ';'),
        file = outfile, append = TRUE)
  write('hyperShade -assign Color_Presets:Bone;',
        file = outfile, append = TRUE)
  write(paste0('hide ', file_prefix, ';\n'),
               file = outfile, append = TRUE)

  # pmap() along rows of data
  pmap(data,
       write_arrows)
}

write_arrows <- function(muscle, side, x_origin, y_origin, z_origin,
                         x_insertion, y_insertion, z_insertion, force) {

}

stl <- "mystlfile.stl"
data <- read_csv(system.file("extdata",
                             "AL_008_data.csv",
                             package = "MuscleTernary"))
