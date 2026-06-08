# Calculate the relative proportion of the items of a given vector

Calculate the relative proportion of the items of a given vector

## Usage

``` r
relative_proportion(x)
```

## Arguments

- x:

  Vector of length 3

## Value

A normalized unit vector of length 3

## Examples

``` r
relative_proportion(c(1, 2, 3))
#> [1] 0.07142857 0.28571429 0.64285714
relative_proportion(c(3, 4, 0))
#> [1] 0.36 0.64 0.00
```
