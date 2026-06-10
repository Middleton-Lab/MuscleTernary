.check_mel_cols <- function(data) {
  col_needed <- c(
    "x_origin", "y_origin", "z_origin",
    "x_insertion", "y_insertion", "z_insertion",
    "muscle", "force", "side"
  )
  if (length(intersect(col_needed, names(data))) != 9) {
    stop(strwrap(
      'Columns must include "x_origin", "y_origin", "z_origin",
       "x_insertion", "y_insertion", "z_insertion",
       "muscle", "side", "force"'
    ))
  }
}

.setup_mel_file_info <- function(stl, outfile, rev_arrows) {
  file_prefix <- stringr::str_sub(stl, start = 1L, end = -5L)
  stl_path <- file.path(getwd(), stl)
  if (is.null(outfile)) outfile <- paste0(file_prefix, ".mel")
  message("Assuming that the stl file is found at ", stl_path, "\n")
  message("Writing .mel file to ", outfile, "\n")
  if (rev_arrows) {
    message(strwrap(
      "Reversing the arrowheads. If they point the wrong
       direction, change rev_arrows to FALSE.",
      prefix = " ", initial = ""
    ), "\n")
  } else {
    message(strwrap(
      "Not reversing the arrowheads. If they point the wrong
       direction, change rev_arrows to TRUE.",
      prefix = " ", initial = ""
    ), "\n")
  }
  return(list(file_prefix = file_prefix, stl_path = stl_path,
              outfile = outfile))
}

.setup_mel_radii <- function(data, scale_radius, use_stl, stl,
                             max_radius) {
  if (scale_radius) {
    if (use_stl) {
      centroid <- centroid_size(stl)
      max_radius <- 1.126e-01 + 6.488e-05 * centroid
    }
    data <- data |>
      dplyr::mutate(cylinder_r = force / max(force) * max_radius)
  } else {
    data <- data |>
      dplyr::mutate(cylinder_r = max_radius / 2)
  }
  message(
    "Note: the ratio of max to min forces is ",
    round(max(data$force) / min(data$force), 3), "\n"
  )
  message(strwrap(paste0(
    "The maximum cylinder width is ",
    round(max(data$cylinder_r), 3),
    ". Adjust this based on the size of your model.\n"
  )))
  data$cone_r <- data$cylinder_r * 2
  data$cone_hr <- 2
  return(data)
}

.write_mel_header <- function(outfile, data) {
  write(glue::glue("// File: {outfile}"), file = outfile)
  write(
    glue::glue("// Generated: ",
               "{format(Sys.time(), '%a %b %d %H:%M:%S %Y')}"),
    file = outfile, append = TRUE
  )
  write(
    glue::glue("// Note: the ratio of max to min forces is ",
               "{round(max(data$force) / min(data$force), 3)}"),
    file = outfile, append = TRUE
  )
  write('', file = outfile, append = TRUE)
}

.load_mel_shader <- function(shader_file, data, write_file,
                             outfile) {
  if (shader_file == "default") {
    shader <- readr::read_csv(
      system.file("extdata", "muscle_colors.csv",
                  package = "MuscleTernary"),
      show_col_types = FALSE
    )
  } else {
    if (!endsWith(shader_file, "csv")) {
      stop("shader_file should be csv.")
    }
    shader <- readr::read_csv(shader_file,
                              show_col_types = FALSE) |>
      dplyr::mutate(muscle = stringr::str_replace_all(
        muscle, stringr::fixed(" "), "_"
      ))
  }
  missing_from_shader <-
    data$muscle[!(data$muscle %in% shader$muscle)]
  if (rlang::is_empty(missing_from_shader)) {
    if (write_file) generate_shader(shader, outfile)
  } else {
    message("Muscles in data missing from shader: \n")
    message(paste(missing_from_shader, collapse = "\n"))
    stop()
  }
}

.write_stl_import <- function(outfile, file_prefix, stl_path) {
  write('\n// Import stl model', file = outfile, append = TRUE)
  write(
    glue::glue(
      'file -import -type "STLImport" -ignoreVersion -ra true ',
      '-mergeNamespacesOnClash false -namespace "{file_prefix}" ',
      '-pr "{stl_path}";'
    ),
    file = outfile, append = TRUE
  )
  write(glue::glue('select -r {file_prefix};'),
        file = outfile, append = TRUE)
  write('sets -e -forceElement BoneSG;',
        file = outfile, append = TRUE)
  write('', file = outfile, append = TRUE)
}

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
#' scaled based on the force variable?
#' @param use_stl boolean (default \code{FALSE}. Should scaling use the
#' maximum value automatically based on the stl centroid size?
#' @param max_radius numeric Maximum radius value. Ignored if
#' \code{scale_radius} is \code{TRUE}.
#' @param rev_arrows boolean (default \code{TRUE}) Should the arrowheads be
#' reversed (you probably want \code{TRUE})
#' @param write_file boolean (default \code{TRUE}) Should the mel file be
#' written out
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of
#'   writing a Maya mel script file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data_file <- system.file("extdata", "AL_008_data.csv",
#'                          package = "MuscleTernary")
#' dat <- readr::read_csv(data_file)
#' make_mel("L_mPTd_Or.stl", dat)
#' }
make_mel <- function(stl,
                     data,
                     shader_file = "default",
                     outfile = NULL,
                     scale_radius = TRUE,
                     use_stl = FALSE,
                     max_radius = 8,
                     rev_arrows = TRUE,
                     write_file = TRUE) {
  .check_mel_cols(data)

  info <- .setup_mel_file_info(stl, outfile, rev_arrows)
  file_prefix <- info$file_prefix
  stl_path <- info$stl_path
  outfile <- info$outfile

  data <- .setup_mel_radii(data, scale_radius, use_stl, stl,
                           max_radius)

  if (write_file) {
    .write_mel_header(outfile, data)
  }

  .load_mel_shader(shader_file, data, write_file, outfile)

  if (write_file) {
    .write_stl_import(outfile, file_prefix, stl_path)
    purrr::pwalk(data, write_arrows,
                 outfile = outfile,
                 rev_arrows = rev_arrows)
  }

  return(invisible(NULL))
}
