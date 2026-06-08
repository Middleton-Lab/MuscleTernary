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

## Examples

``` r
row <- data.frame(x_1 = 0, y_1 = 0, z_1 = 0,
                  x_2 = 1, y_2 = 1, z_2 = 1)
interpolate_ternary(row, length_out = 5)
#>   x_1 y_1 z_1 x_2 y_2 z_2    x    y    z .frame
#> 1   0   0   0   1   1   1 0.00 0.00 0.00      1
#> 2   0   0   0   1   1   1 0.25 0.25 0.25      2
#> 3   0   0   0   1   1   1 0.50 0.50 0.50      3
#> 4   0   0   0   1   1   1 0.75 0.75 0.75      4
#> 5   0   0   0   1   1   1 1.00 1.00 1.00      5
```
