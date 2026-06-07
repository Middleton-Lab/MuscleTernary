# Estimate PCSA from from a pair of muscle attachment stl meshes.

Estimate PCSA from from a pair of muscle attachment stl meshes.

## Usage

``` r
pcsa(
  stl1,
  stl2,
  fascicle_length = 1,
  theta = 0,
  units_adjust = 1,
  stl_area = TRUE
)
```

## Arguments

- stl1:

  String: Path to stl.

- stl2:

  String: Path to stl.

- fascicle_length:

  Numeric: Fascicle length

- theta:

  Numeric: Fascicle angle (radians)

- units_adjust:

  Numeric: Multiplier adjustment if units are not in mm.

- stl_area:

  Boolean: Use the area of the stl mesh (default) or use the centroid
  size.

## Value

numeric: Estimate of PCSA for the muscle defined by the two attachments
of \`stl1\` and \`stl2\`.
