# Calculate centroid size for an stl file

Calculate centroid size for an stl file

## Usage

``` r
centroid_size(fname)
```

## Arguments

- fname:

  String: Path to stl file

## Value

Numeric: Centroid size

## Examples

``` r
f <- system.file("extdata", "L_mPTd_Or.stl",
                 package = "MuscleTernary")
centroid_size(f)
#> [1] 1527.429
```
