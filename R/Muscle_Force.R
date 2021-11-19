#' Estimate muscle force
#'
#' Use PCSA and specific tension to estimate muscle force
#'
#' @param PCSA numeric: Value for PCSA (e.g., from \code{PCSA()})
#' @param Tspec numeric: Value for specific tension of muscle in cubic mm.
#'
#' @return numeric: Estimate of muscle force
#'
#' @export
#'
Muscle_Force <- function(PCSA, Tspec = 0.3) {
  return(PCSA * Tspec)
}
