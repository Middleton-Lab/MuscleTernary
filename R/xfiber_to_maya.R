#' Generate Maya mel file from xfiber tracing
#'
#' @param fname string: Path to excel file
#' @param outfile string: Path to output file
#' @param radius numeric: Radius of the cylinders in Maya
#' @param n numeric: Number of tracks to randomly select
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of
#'   writing a Maya mel script file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' f <- system.file("extdata", "AV069_SC.xml",
#'                  package = "MuscleTernary")
#' xfiber_to_maya(f, outfile = tempfile(fileext = ".mel"))
#' }
xfiber_to_maya <- function(fname, outfile, radius = 8, n = NULL) {
  Tracks <- read_xfiber(fname)

  if (!is.null(n)) {
    if (n > length(unique(Tracks$track_num))) {
      stop("n is greater than the number of unique tracks")
    }

    Tracks <- Tracks |>
      dplyr::filter(track_num %in%
                      sample(unique(Tracks$track_num), size = n))
  }

  # Generate ID for each segment
  Tracks <- Tracks |>
    dplyr::mutate(track_num = paste0("tr_", track_num),
                  pt_pair = stringr::str_replace(
                    pt_pair, stringr::fixed(","), "_")) |>
    tidyr::unite("ID", track_num, pt_pair)

  # Drop Orientation columns
  Tracks <- Tracks |>
    dplyr::select(-OrientationTheta, -OrientationPhi)

  message("Writing .mel file to ", outfile, "\n")

  # Write header info
  write(glue::glue("// File: {outfile}"), file = outfile)
  write(glue::glue("// Generated: ",
                   "{format(Sys.time(), '%a %b %d %H:%M:%S %Y')}"),
        file = outfile, append = TRUE)

  write('', file = outfile, append = TRUE)

  # Process Tracks link by line, creating segments in Maya mel file
  purrr::pwalk(.l = Tracks,
               .f = write_segment,
               outfile = outfile,
               radius = radius)

  return(invisible(NULL))
}
