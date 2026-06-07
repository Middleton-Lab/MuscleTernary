# Write mel file

Write mel file

## Usage

``` r
make_mel(
  stl,
  data,
  shader_file = "default",
  outfile = NULL,
  scale_radius = TRUE,
  use_stl = FALSE,
  max_radius = 8,
  rev_arrows = TRUE,
  write_file = TRUE
)
```

## Arguments

- stl:

  string Location of the stl file. Assumed to present in the working
  directory

- data:

  data.frame Object with the data

- shader_file:

  string Either "default" to use the standard palette, or the path to a
  csv file with colors for each muscle.

- outfile:

  string Name of file to write to. Defaults to the base name of the stl
  file.

- scale_radius:

  boolean (default `TRUE`) Should the radius be scaled based on the
  force variable?

- use_stl:

  boolean (default `FALSE`. Should scaling use the maximum value
  automatically based on the stl centroid size?

- max_radius:

  numeric Maximum radius value. Ignored if `scale_radius` is `TRUE`.

- rev_arrows:

  boolean (default `TRUE`) Should the arrowheads be reversed (you
  probably want `TRUE`)

- write_file:

  boolean (default `TRUE`) Should the mel file be written out
