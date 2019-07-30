#' Muscle Ternary
#'
#' More description
#'
#' \tabular{ll}{ Package: \tab MuscleTernary\cr
#'               Type: \tab Package\cr
#'               Version: \tab 0.18\cr
#'               Date: \tab 2019-07-30\cr
#'               License: \tab GPL\cr
#'               LazyLoad: \tab yes\cr
#'               LazyData: \tab yes\cr }
#'
#' @name MuscleTernary-package
#' @aliases MuscleTernary-package MuscleTernary
#' @docType package
#' @author Kevin M. Middleton (\url{middletonk@@missouri.edu})
#' @keywords package
#' @import ggtern readxl tidyverse animation rlist dplyr
#' @importFrom stats complete.cases
#' @importFrom utils globalVariables assignInNamespace
#' @importFrom ggplot2 ggproto
#' @importFrom stringr str_detect
NULL

# Ignore some global variables
utils::globalVariables(c("matches",
                         "muscle",
                         ".",
                         "coord_transform",
                         "scale_color_manual",
                         "scale_fill_manual",
                         "x",
                         "y",
                         "z"))
