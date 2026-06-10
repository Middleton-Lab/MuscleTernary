# MuscleTernary

<!-- badges: start -->
[![R-CMD-check](https://github.com/Middleton-Lab/MuscleTernary/actions/workflows/check-release.yaml/badge.svg)](https://github.com/Middleton-Lab/MuscleTernary/actions/workflows/check-release.yaml)
[![Codecov test coverage](https://codecov.io/gh/Middleton-Lab/MuscleTernary/graph/badge.svg)](https://app.codecov.io/gh/Middleton-Lab/MuscleTernary)
<!-- badges: end -->

MuscleTernary provides tools for visualizing 3D muscle force resultants in
ternary space. From muscle origin and insertion coordinates — optionally
with force or moment data and any number of categorical or continuous
variables — it computes the relative *x*, *y*, *z* proportions of each
muscle's line of action and plots them on a ternary diagram via extensions
of the [`ggtern`](https://CRAN.R-project.org/package=ggtern) package.

The package also:

- reads and reduces Avizo Xfiber fiber-tracking data (`read_xfiber_xml()`,
  `find_track_ends()`, `ends_to_vectors()`);
- works with `.stl` meshes to compute centroids, centroid size, surface
  area, PCSA, and muscle force (`read_stl()`, `centroid_location()`,
  `centroid_size()`, `stl_area()`, `pcsa()`, `muscle_force()`);
- measures pennation angle relative to a central axis
  (`central_axis()`, `pennation_angle()`); and
- generates Maya MEL scripts to render models and force-vector arrows
  (`make_mel()`, `xfiber_to_maya()`).

## Installation

- Install `pak` (if you haven't already): `install.packages("pak")`.
- Install MuscleTernary from GitHub:
  `pak::pak("Middleton-Lab/MuscleTernary")`.

## Quick start

```r
library(MuscleTernary)
library(ggtern)

# Muscle origin/insertion coordinates, with side and force
coords <- readr::read_csv(
  system.file("extdata", "AL_008_data.csv", package = "MuscleTernary"),
  show_col_types = FALSE
)

# Convert 3D resultant vectors to ternary (x, y, z) proportions
tern <- coords_to_ternary(coords, grouping = c("muscle", "side"))

# Plot in ternary space
ggtern(tern, aes(x, y, z, color = muscle)) +
  geom_point(size = 3) +
  muscle_color_map()
```

See `?MuscleTernary`, `demo(ternary_plot)`, and the package vignettes
(`browseVignettes("MuscleTernary")`) for fuller workflows, including
Xfiber data, animation, and Maya MEL export.
