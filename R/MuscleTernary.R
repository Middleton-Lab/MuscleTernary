#' Muscle Ternary
#'
#' More description
#'
#' \tabular{ll}{ Package: \tab MuscleTernary\cr
#'               Type: \tab Package\cr
#'               Version: \tab 0.13\cr
#'               Date: \tab 2017-04-04\cr
#'               License: \tab GPL\cr
#'               LazyLoad: \tab yes\cr
#'               LazyData: \tab yes\cr }
#'
#' @name MuscleTernary-package
#' @aliases MuscleTernary-package MuscleTernary
#' @docType package
#' @author Kevin M. Middleton (\url{middletonk@@missouri.edu})
#' @keywords package
#' @import ggtern readxl dplyr animation
#' @importFrom stats complete.cases
#' @importFrom utils globalVariables assignInNamespace
#' @importFrom ggplot2 ggproto
NULL

# Ignore some global variables
utils::globalVariables(c("matches", "muscle", ".", "Muscle_Group",
                         "Left_Right", "coord_transform",
                         "scale_color_manual", "x", "y", "z"))
