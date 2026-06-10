# MuscleTernary

MuscleTernary provides tools for visualizing 3D muscle force resultants
in ternary space. From muscle origin and insertion coordinates —
optionally with force or moment data and any number of categorical or
continuous variables — it computes the relative *x*, *y*, *z*
proportions of each muscle’s line of action and plots them on a ternary
diagram via extensions of the
[`ggtern`](https://CRAN.R-project.org/package=ggtern) package.

The package also:

- reads and reduces Avizo Xfiber fiber-tracking data
  ([`read_xfiber_xml()`](https://middleton-lab.github.io/MuscleTernary/reference/read_xfiber_xml.md),
  [`find_track_ends()`](https://middleton-lab.github.io/MuscleTernary/reference/find_track_ends.md),
  [`ends_to_vectors()`](https://middleton-lab.github.io/MuscleTernary/reference/ends_to_vectors.md));
- works with `.stl` meshes to compute centroids, centroid size, surface
  area, PCSA, and muscle force
  ([`read_stl()`](https://middleton-lab.github.io/MuscleTernary/reference/read_stl.md),
  [`centroid_location()`](https://middleton-lab.github.io/MuscleTernary/reference/centroid_location.md),
  [`centroid_size()`](https://middleton-lab.github.io/MuscleTernary/reference/centroid_size.md),
  [`stl_area()`](https://middleton-lab.github.io/MuscleTernary/reference/stl_area.md),
  [`pcsa()`](https://middleton-lab.github.io/MuscleTernary/reference/PCSA.md),
  [`muscle_force()`](https://middleton-lab.github.io/MuscleTernary/reference/Muscle_Force.md));
- measures pennation angle relative to a central axis
  ([`central_axis()`](https://middleton-lab.github.io/MuscleTernary/reference/central_axis.md),
  [`pennation_angle()`](https://middleton-lab.github.io/MuscleTernary/reference/pennation_angle.md));
  and
- generates Maya MEL scripts to render models and force-vector arrows
  ([`make_mel()`](https://middleton-lab.github.io/MuscleTernary/reference/make_mel.md),
  [`xfiber_to_maya()`](https://middleton-lab.github.io/MuscleTernary/reference/xfiber_to_maya.md)).

## Installation

- Install `pak` (if you haven’t already): `install.packages("pak")`.
- Install MuscleTernary from GitHub:
  `pak::pak("Middleton-Lab/MuscleTernary")`.

## Quick start

``` r

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

See
[`?MuscleTernary`](https://middleton-lab.github.io/MuscleTernary/reference/MuscleTernary-package.md),
`demo(ternary_plot)`, and the package vignettes
(`browseVignettes("MuscleTernary")`) for fuller workflows, including
Xfiber data, animation, and Maya MEL export.
