# Define muscle force axis vector

Calculate the force axis for a given muscle using two points. Points are
assumed to be the centroids of origin and insertion, but can be other
points as needed.

## Usage

``` r
central_axis(origin_centroid, insertion_centroid)
```

## Arguments

- origin_centroid::

  numeric vector of origin centroid, or equivalent point

- insertion_centroid::

  numeric vector of insertion centroid, or equivalent point

## Value

Named numeric vector of the axis between the origin and insertion
centroids

## Examples

``` r

central_axis(c(0, 0, 0), c(1, 1, 1))
#>          x          y          z 
#> -0.5773503 -0.5773503 -0.5773503 
```
