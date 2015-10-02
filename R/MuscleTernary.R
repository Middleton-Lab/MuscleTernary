#' Muscle Ternary
#'
#' More description
#'
#' \tabular{ll}{ Package: \tab MuscleTernary\cr
#'               Type: \tab Package\cr
#'               Version: \tab 0.1.0\cr
#'               Date: \tab 2015-10-01\cr
#'               License: \tab GPL\cr
#'               LazyLoad: \tab yes\cr
#'               LazyData: \tab yes\cr }
#'
#' @name MuscleTernary-package
#' @aliases MuscleTernary-package MuscleTernary
#' @docType package
#' @author Kevin M. Middleton (\url{middletonk@@missouri.edu})
#' @keywords package
#' @import ggplot2 ggtern readxl
#' @examples
#' library("MuscleTernary")
#'
#' M <- read_excel(system.file("extdata", "AL_700_attachments.xlsx", package="MuscleTernary"),
#'                 sheet = "Sheet1")
#'
#' colnames(M) <- c("muscle", "x_origin", "y_origin", "z_origin", "x_insertion",
#'                  "y_insertion", "z_insertion")
#' rownames(M) <- M[ , 1]
#' M <- M[, 2:7]
#'
#' coords_or <- M[, c("x_origin", "y_origin", "z_origin")]
#' coords_ins <- M[, c("x_insertion", "y_insertion", "z_insertion")]
#'
#' #Calculating vector from origin to insertion.
#' vectors <- as.matrix(coords_or) - as.matrix(coords_ins)
#' colnames(vectors) <- c("x", "y", "z")
#'
#' # Pass rows sequentially to make_unit_vector and relative proportion
#' unit_vectors <- t(apply(vectors, 1, make_unit_vector))
#' prop_vectors <- t(apply(vectors, 1, relative_proportion))
#'
#' # Need a data.frame for ggplot.
#' df_to_plot <- as.data.frame(prop_vectors)
#'
#' # Scale to percentage
#' df_to_plot <- df_to_plot * 100
#'
#' # Add some random uniform
#' df_to_plot$size <- runif(nrow(df_to_plot), 0, 1)
#'
#' ggtern(df_to_plot, aes(x, y, z, size = size)) +
#'   geom_point() +
#'   theme_showarrows()
#'
NULL


