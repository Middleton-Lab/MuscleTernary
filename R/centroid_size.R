library(rgl)
library(tidyverse)

centroid_size <- function(ply){
  p <- read.ply(ply, ShowSpecimen = FALSE)$vb %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select(ends_with("pts"))

  centroid <- colMeans(p)

  sqrt(sum((centroid[1] - p$xpts) ^ 2 +
             (centroid[2] - p$ypts) ^ 2 +
             (centroid[3] - p$zpts) ^ 2))
}

ply <- "~/Downloads/AL_008.ply"
centroid_size(ply)

centroid_size <- function(stl) {
  st <- rgl::readSTL(stl, plot = FALSE)
  centroid <- colMeans(st)
  cs <- sqrt(sum((centroid[1] - st[, 1]) ^ 2 +
                   (centroid[2] - st[, 2]) ^ 2 +
                   (centroid[3] - st[, 3]) ^ 2))
  return(cs)
}

centroid_size("~/Downloads/gomphosuchus.stl")
