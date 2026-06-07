# Interpolate (x, y, z) coordinates

Given a start and end point for a pair of xyz coordinate, return the
linearly interpolated data.frame between them. This functional also adds
a \`.frame\` column that allows animation.

## Usage

``` r
interpolate_xyz(start, end, length_out)
```

## Arguments

- start:

  Vector of xyz coordinate for start position.

- end:

  Vector of xyz coordinate for end position.

- length_out:

  Total length of output \`data.frame\`.

## Value

\`data.frame\` interpolated between \`start\` and \`end\` with column
for frame number.
