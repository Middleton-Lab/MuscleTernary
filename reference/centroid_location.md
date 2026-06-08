# Calculate centroid location

Calculate centroid location

## Usage

``` r
centroid_location(fname)
```

## Arguments

- fname:

  String: Path to stl file

## Value

Numeric vector of centroid location

## Examples

``` r
f <- system.file("extdata", "L_mPTd_Or.stl",
                 package = "MuscleTernary")
centroid_location(f)
#> [1]   26.68014   21.37667 -455.58045
```
