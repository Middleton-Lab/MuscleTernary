#' Holliday Lab Muscle Color Map
#'
#' This function returns a \code{scale_color_manual()} that matches
#' the Holliday lab muscle color map.
#'
#' @return A \code{scale_color_manual()} that can be added to a
#'   ggplot
#'
#' @export
#'
muscle_color_map <- function(){

  cmap <- scale_color_manual(
    name = "Muscle",
    values = c("mAMES" = "#0000FF",
               "mAMEM" = "#55FFFF",
               "mAMEP" = "#8080FF",
               "mAMP" = "#00FF00",
               "mPSTs" = "#FF80FF",
               "mPSTp" = "#400080",
               "mPTd" = "#FF8000",
               "mPTv" = "#FF0000",
               "mPPt" = "#808080",
               "mDM" = "#FF0080",
               "mLPt" = "#09C4A8",
               "mEM" = "#FFFF00",
               "mPM" = "#7DB7E6"))

  return(cmap)
}
