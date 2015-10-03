#' Muscle Ternary
#'
#' More description
#'
#' \tabular{ll}{ Package: \tab MuscleTernary\cr
#'               Type: \tab Package\cr
#'               Version: \tab 0.5.0\cr
#'               Date: \tab 2015-10-02\cr
#'               License: \tab GPL\cr
#'               LazyLoad: \tab yes\cr
#'               LazyData: \tab yes\cr }
#'
#' @name MuscleTernary-package
#' @aliases MuscleTernary-package MuscleTernary
#' @docType package
#' @author Kevin M. Middleton (\url{middletonk@@missouri.edu})
#' @keywords package
#' @import ggplot2 ggtern readxl dplyr
#' @importFrom stats complete.cases
#' @importFrom utils globalVariables
NULL

# Ignore some global variables
utils::globalVariables(c("matches", "muscle", "."))
