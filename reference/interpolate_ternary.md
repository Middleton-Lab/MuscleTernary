# Interpolate between two point in a coordinates object.

Interpolate between two point in a coordinates object.

## Usage

``` r
interpolate_ternary(x, length_out = 200)
```

## Arguments

- x:

  Row of a coordinates file. Should contain columns \`x_1\`, \`y_1\`,
  \`z_1\`, \`x_2\`, \`y_2\`, \`z_2\`, for example in the output of
  \`merge_coords()\`.

- length_out:

  Total length of output \`data.frame\`.

## Value

\`data.frame\` with interpolated points added. All other variables are
copied to fill.
