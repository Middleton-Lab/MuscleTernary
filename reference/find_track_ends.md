# Reduce Xfiber tracks to vectors connecting the two endpoints

This function finds the (x, y, z) coordinates of the starting and ending
points of an Xfiber tracks object read by \`read_xfiber_xml()\`.

## Usage

``` r
find_track_ends(Tracks)
```

## Arguments

- Tracks:

  data.frame: Object containing the full set of tracks and a column for
  \`muscle\`, which is required by \`coords_to_ternary()\`.

## Value

tibble: \`Tracks\` but reduced to one row per track.

## Examples

``` r
D <- read_xfiber_xml(system.file("extdata",
                     "AV069_SC.xml",
                     package = "MuscleTernary")) |>
                     mutate(muscle = "SC")
ends <- find_track_ends(D)
```
