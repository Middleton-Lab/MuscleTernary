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

  muscle_names <- c("mAMES", "mAMEM", "mAMEP", "mAMP", "mPSTs", "mPSTp",
                    "mPTd", "mPTv", "mPPt", "mDM", "mLPT", "mEM",
                    "mPM")
  color_palette <- c("#0000FF", "#55FFFF", "#8080FF", "#00FF00",
                     "#FF80FF", "#400080", "#FF8000", "#FF0000",
                     "#808080", "#FF0080", "#09C4A8", "#FFFF00",
                     "#7DB7E6")

  return(scale_color_manual(labels = muscle_names,
                            values = color_palette,
                            name = "Muscle"))
}
