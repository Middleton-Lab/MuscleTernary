#' Reduce Xfiber tracks to vectors connecting the two endpoints
#'
#' This function finds the (x, y, z) coordinates of the starting and ending
#' points of an Xfiber tracks object read by `read_xfiber_xml()`.
#'
#' @param Tracks data.frame: Object containing the full set of tracks and a
#' column for `muscle`, which is required by `coords_to_ternary()`.
#'
#' @return tibble: `Tracks` but reduced to one row per track.
#'
#' @export
#'
#' @examples
#' D <- read_xfiber_xml(system.file("extdata",
#'                      "AV069_SC.xml",
#'                      package = "MuscleTernary")) |>
#'                      mutate(muscle = "SC")
#' ends <- find_track_ends(D)

find_track_ends <- function(Tracks) {
  # Check that there is a column `muscle`
  if (!("muscle" %in% names(Tracks))) {
    stop("No column `muscle` detected in Tracks")
  }

  Track_ends <- purrr::map(
    .x = unique(Tracks$track_num),
    .f = function(ID, Tracks) {
      T_sub <- Tracks |>
        dplyr::filter(track_num == ID)
      st <- slice_head(T_sub)
      en <- slice_tail(T_sub)

      return(tibble::tibble(
        muscle = st$muscle,
        track_num = ID,
        x_origin = st$x_origin,
        y_origin = st$y_origin,
        z_origin = st$z_origin,
        x_insertion = en$x_insertion,
        y_insertion = en$y_insertion,
        z_insertion = en$z_insertion,
        OrientationTheta = st$OrientationTheta,
        OrientationPhi = st$OrientationPhi
      ))
    },
    Tracks = Tracks
  ) |>
    purrr::list_rbind()

  return(Track_ends)
}
