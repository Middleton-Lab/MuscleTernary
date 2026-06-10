# Process coordinates data.frame

Process coordinates data.frame

## Usage

``` r
coords_to_ternary(coords, grouping = NULL)
```

## Arguments

- coords:

  \`data.frame\` or \`tibble\` containing coordinates and other columns
  to be plotted in ternary space. Required columns are at least
  "muscle", "x_origin", "y_origin", "z_origin", "x_insertion",
  "y_insertion", and "z_insertion"

- grouping:

  Character vector of grouping variables for calculating means. Defaults
  to \`NULL\`, which will return all the original rows and columns

## Value

`data.frame` suitable for plotting with
[`ggtern()`](https://rdrr.io/pkg/ggtern/man/ggtern.html).

## Examples

``` r
coords <- data.frame(
  muscle = "mPTd",
  x_origin = 10, y_origin = 20, z_origin = 30,
  x_insertion = 5, y_insertion = 15, z_insertion = 25
)
coords_to_ternary(coords)
#>   muscle        x        y        z
#> 1   mPTd 33.33333 33.33333 33.33333
```
