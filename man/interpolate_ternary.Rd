% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpolate_ternary.R
\name{interpolate_ternary}
\alias{interpolate_ternary}
\title{Interpolate between two point in a coordinates object.}
\usage{
interpolate_ternary(x, length_out = 200)
}
\arguments{
\item{x}{Row of a coordinates file. Should contain columns `x_1`,
`y_1`, `z_1`, `x_2`, `y_2`, `z_2`, for example in the output of
`merge_coords()`.}

\item{length_out}{Total length of output `data.frame`.}
}
\value{
`data.frame` with interpolated points added. All other
  variables are copied to fill.
}
\description{
Interpolate between two point in a coordinates object.
}
