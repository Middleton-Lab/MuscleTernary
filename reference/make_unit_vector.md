# Create a unit vector from a given vector

Create a unit vector from a given vector

## Usage

``` r
make_unit_vector(x)
```

## Arguments

- x:

  Vector of length 3.

## Value

A normalized unit vector of length 3

## Examples

``` r
make_unit_vector(c(3, 4, 0))
#> [1] 0.6 0.8 0.0
make_unit_vector(c(1, 1, 1))
#> [1] 0.5773503 0.5773503 0.5773503
```
