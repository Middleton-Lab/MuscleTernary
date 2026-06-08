# Calculate area of an stl mesh

Calculate area of an stl mesh

## Usage

``` r
stl_area(fname)
```

## Arguments

- fname:

  String: path to stl file

## Value

Numeric: Area of the stl mesh

## Examples

``` r
f <- system.file("extdata", "L_mPTd_Or.stl",
                 package = "MuscleTernary")
stl_area(f)
#> [1] 478.8399
```
