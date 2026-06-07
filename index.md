# Installation

## Preliminaries

- Install `remotes` (if you haven’t already):
  `install.packages("remotes")`

You might also need to install Rtools
(<https://cran.r-project.org/bin/windows/Rtools/>) if the installation
step below does not work correctly.

## For initial installation or updates

- Install MuscleTernary from github:
  - `remotes::install_github("Middleton-Lab/MuscleTernary", dependencies = TRUE, build_vignettes = TRUE)`

View
[`?MuscleTernary`](https://middleton-lab.github.io/MuscleTernary/reference/MuscleTernary-package.md)
or `demo(ternary_plot)` for a demo.
