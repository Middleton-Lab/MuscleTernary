#' Muscle Ternary
#'
#' More description
#'
#' \tabular{ll}{ Package: \tab MuscleTernary\cr
#'               Type: \tab Package\cr
#'               Version: \tab 0.21\cr
#'               Date: \tab 2022-03-02\cr
#'               License: \tab GPL\cr
#'               LazyLoad: \tab yes\cr
#'               LazyData: \tab yes\cr }
#'
#' @name MuscleTernary-package
#' @aliases MuscleTernary-package MuscleTernary
#' @docType package
#'
#' @author Kevin M. Middleton (\url{middletonk@@missouri.edu})
#'
#' @keywords package internal
#'
## usethis namespace: start
#' @import ggtern readxl tidyverse animation rlist dplyr
#' @importFrom ggplot2 ggproto
#' @importFrom purrr pmap
#' @importFrom stats complete.cases
#' @importFrom stringr str_detect
#' @importFrom utils globalVariables assignInNamespace
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_children
#' @importFrom xml2 xml_contents
#' @importFrom xml2 xml_text
## usethis namespace: end
"_PACKAGE"
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
                         "z",
                         "OrientationPhi",
                         "OrientationTheta",
                         "pt_pair",
                         "track_num"))

