# Function turning xfiber endpoint vectors into unit vectors Should be used following 'find_track_ends'

Function turning xfiber endpoint vectors into unit vectors Should be
used following 'find_track_ends'

## Usage

``` r
ends_to_vectors(coords, grouping = NULL)
```

## Arguments

- coords::

  \`data.frame\` or \`tibble\` containing coordinates and other columns
  to be converted into unit vectors. Required columns are at least
  "muscle", "x_origin", "y_origin", "z_origin", "x_insertion",
  "y_insertion", and "z_insertion".

- grouping::

  Character vector of grouping variables for calculating means. Defaults
  to \`NULL\`, which will return all the original rows and columns

## Value

'data.frame' with x, y, and z components of unit vectors of each fiber
track

## Examples

``` r
D <- read_xfiber_xml(system.file("extdata",
                     "AV069_SC.xml",
                     package = "MuscleTernary")) |>
                     mutate(muscle = "SC")
ends <- find_track_ends(D)

ends_to_vectors(ends)
#> # A tibble: 235 × 7
#>    muscle track_num OrientationTheta OrientationPhi      x      y      z
#>    <chr>      <dbl>            <dbl>          <dbl>  <dbl>  <dbl>  <dbl>
#>  1 SC             0             73.0           32.5 -0.816 -0.505 -0.281
#>  2 SC             1             83.0           35.4 -0.817 -0.564 -0.120
#>  3 SC             2             33.4           48.3 -0.365 -0.412 -0.835
#>  4 SC             3             30.3           41.2 -0.380 -0.333 -0.863
#>  5 SC             4             39.6           37.3 -0.506 -0.383 -0.773
#>  6 SC             5             29.5           39.1 -0.383 -0.310 -0.870
#>  7 SC             6             48.2           35.0 -0.610 -0.428 -0.667
#>  8 SC             7             19.0           47.5 -0.220 -0.238 -0.946
#>  9 SC             8             36.5           37.4 -0.467 -0.352 -0.811
#> 10 SC             9             44.8           36.9 -0.565 -0.424 -0.708
#> # ℹ 225 more rows

```
