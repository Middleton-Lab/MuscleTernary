#' Read file from Avizo xfiber
#'
#' By default, xfiber write an excel file in XML format. Loading such a file
#' and the resaving as xlsx will allow it to be read into R.
#'
#' @param fname string: file name to load (should be xlsx).
#'
#' @return tibble:
#'
#' @export
#'
read_xfiber <- function(fname) {
  Nodes <- readxl::read_excel(fname, sheet = 1)
  Points <- readxl::read_excel(fname, sheet = 2)
  Segments <- readxl::read_excel(fname, sheet = 3)

  track_length <- nrow(Points) - length(unique(Segments$`Segment ID`))

  Tracks <- tibble(
    track_num = numeric(length = track_length),
    pt_pair = character(length = track_length),
    x_origin = numeric(length = track_length),
    y_origin = numeric(length = track_length),
    z_origin = numeric(length = track_length),
    x_insertion	 = numeric(length = track_length),
    y_insertion = numeric(length = track_length),
    z_insertion = numeric(length = track_length),
    OrientationTheta = numeric(length = track_length),
    OrientationPhi = numeric(length = track_length)
  )

  ctr <- 1L

  for (ii in 1:nrow(Segments)) {
    x <- Segments[ii, ]
    pts <- stringr::str_split(x$`Point IDs`, ",")[[1]] %>%
      as.numeric()
    P <- tibble(p1 = pts[1:(length(pts) - 1)],
                p2 = pts[2:(length(pts))])
    for (jj in 1:nrow(P)) {
      p <- P[jj, ]
      Tracks$track_num[ctr] <- x$`Segment ID`
      Tracks$pt_pair[ctr] <- paste(p$p1, p$p2, sep = ",")
      Tracks$x_origin[ctr] <- Points$`X Coord`[Points$`Point ID` == p$p1]
      Tracks$y_origin[ctr] <- Points$`Y Coord`[Points$`Point ID` == p$p1]
      Tracks$z_origin[ctr] <- Points$`Z Coord`[Points$`Point ID` == p$p1]
      Tracks$x_insertion[ctr] <- Points$`X Coord`[Points$`Point ID` == p$p2]
      Tracks$y_insertion[ctr] <- Points$`Y Coord`[Points$`Point ID` == p$p2]
      Tracks$z_insertion[ctr] <- Points$`Z Coord`[Points$`Point ID` == p$p2]
      Tracks$OrientationTheta[ctr] <- x$OrientationTheta
      Tracks$OrientationPhi[ctr] <- x$OrientationPhi
      ctr <- ctr + 1L
    }
  }
  return(Tracks)
}
