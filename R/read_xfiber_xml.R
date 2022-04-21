#' Import from Avizo xfiber XML
#'
#' @param filename string: file name to load (should be an XML file).
#'
#' @return tibble: All tracks concatenated.
#'
#' @export
#'
read_xfiber_xml <- function(filename) {
  doc <- xml2::read_xml(filename)

  sheets <- list() # 1 = Nodes, 2 = Points, 3 = Segments
  for (sheet_num in 1:3) {
    sh <- xml2::xml_children(doc)[sheet_num + 1]
    tab <- xml2::xml_children(sh)[1]
    rows <- xml2::xml_children(tab)

    # Column names
    col_names <- xml2::xml_contents(rows[1]) |> xml_text()

    # Pre-allocate matrix
    M <- matrix(data = "", nrow = length(rows) - 1, ncol = length(col_names))

    # Iterate from row 2 until the end, extracting text
    for (ii in 1:nrow(M)) {
      M[ii, ] <- xml2::xml_contents(rows[ii + 1]) |> xml_text()
    }

    M <- as.data.frame(M)
    names(M) <- col_names

    # Convert some columns to numeric
    if (sheet_num == 3) {
      M <- M |>
        dplyr::mutate(across(1:17, as.numeric))
    } else {
      M <- M |>
        dplyr::mutate(across(everything(), as.numeric))
    }

    sheets[[sheet_num]] <- M
  }

  Nodes <- sheets[[1]]
  Points <- sheets[[2]]
  Segments <- sheets[[3]]

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
    pts <- stringr::str_split(x$`Point IDs`, ",")[[1]] |>
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
