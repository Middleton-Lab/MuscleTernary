# Cross-product of two 3d vectors

This function calculates the cross-product of two 3d (3 element)
vectors. Note that this is different from \`base::crossprod()\`.

## Usage

``` r
xprod(a, b)
```

## Arguments

- a:

  numeric vector of length 3

- b:

  numeric vector of length 3

## Value

numeric vector of length 3

## Examples

``` r
xprod(c(0, 1, 2), c(1, 1, 1))
#> [1] -1  2 -1
```
