#' Estimate muscle force
#'
#' Use PCSA and specific tension to estimate muscle force
#'
#' @param PCSA numeric: Value for PCSA (e.g., from \code{pcsa()})
#' @param Tspec numeric: Value for specific tension of muscle in cubic mm.
#' Default of 0.3 is reasonable for mammalian muscle.
#'
#' @return numeric: Estimate of muscle force
#'
#' @export
#'
muscle_force <- function(PCSA, Tspec = 0.3) {
  return(PCSA * Tspec)
}
