#' Generate Maya mel file from xfiber tracing
#'
#' @param fname string: Path to excel file
#' @param outfile string: Path to output file
#' @param radius numeric: Radius of the cylinders in Maya
#' @param n numeric: Number of tracks to randomly select
#'
#' @export
#'
xfiber_to_maya <- function(fname, outfile, radius = 8, n = NULL) {
  Tracks <- read_xfiber(fname)

  if (!is.null(n)) {
    if (n > unique(Tracks$track_num)) {
      stop("n is greater than then number of unique tracks")
    }

    Tracks <- Tracks %>%
      filter(track_num %in% sample(unique(Tracks$track_num), size = n))
  }

  # Generate ID for each segment
  Tracks <- Tracks %>%
    dplyr::mutate(track_num = paste0("tr_", track_num),
                  pt_pair = stringr::str_replace(pt_pair, ",", "_")) %>%
    tidyr::unite("ID", track_num, pt_pair)

  # Drop Orientation columns
  Tracks <- Tracks %>%
    dplyr::select(-OrientationTheta, -OrientationPhi)

  message("Writing .mel file to ", outfile, "\n")

  # Write header info
  write(paste0("// File: ", outfile), file = outfile)
  write(paste0("// Generated: ",
               format(Sys.time(), "%a %b %d %H:%M:%S %Y")),
        file = outfile, append = TRUE)

  write('', file = outfile, append = TRUE)

  # Process Tracks link by line, creating segments in Maya mel file
  nul <- pmap(.l = Tracks,
              .f = write_segment,
              outfile = outfile,
              radius = radius)
}
