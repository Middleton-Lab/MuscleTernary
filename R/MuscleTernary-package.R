#' Muscle Ternary
#'
#' More description
#'
#' \tabular{ll}{ Package: \tab MuscleTernary\cr
#'               Type: \tab Package\cr
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
#' @importFrom ggtern ggtern theme_showarrows
#' @importFrom dplyr across all_of bind_cols contains everything
#' @importFrom dplyr filter group_by group_by_at mutate one_of
#' @importFrom dplyr select slice slice_head slice_tail summarise
#' @importFrom dplyr summarise_all
#' @importFrom ggplot2 ggproto
#' @importFrom grid gpar nullGrob textGrob unit
#' @importFrom tibble tibble
#' @importFrom stats complete.cases
#' @importFrom stringr str_detect
#' @importFrom utils globalVariables assignInNamespace
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_children
#' @importFrom xml2 xml_contents
#' @importFrom xml2 xml_text
## usethis namespace: end
"_PACKAGE"


# Ignore some global variables
utils::globalVariables(c(
    "matches",
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
    "track_num"
))
