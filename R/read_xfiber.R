#' Import from Avizo xfiber Excel
#'
#' By default, xfiber write an excel file in XML format. Loading such a file
#' and the resaving as xlsx will allow it to be read into R with this function.
#'
#' @param filename string: file name to load (should be xlsx).
#'
#' @return tibble: All tracks concatenated.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Requires an xlsx file produced by Avizo xfiber
#' f <- system.file("extdata", "myfibers.xlsx",
#'                  package = "MuscleTernary")
#' read_xfiber(f)
#' }
read_xfiber <- function(filename) {
  Nodes <- readxl::read_excel(filename, sheet = 1)
  Points <- readxl::read_excel(filename, sheet = 2)
  Segments <- readxl::read_excel(filename, sheet = 3)

  track_length <- nrow(Points) - length(unique(Segments$`Segment ID`))

  return(.build_xfiber_tracks(Segments, Points, track_length))
}
