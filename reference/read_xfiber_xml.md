# Import from Avizo xfiber XML

Import from Avizo xfiber XML

## Usage

``` r
read_xfiber_xml(filename)
```

## Arguments

- filename:

  string: file name to load (should be an XML file).

## Value

tibble: All tracks concatenated.

## Examples

``` r
f <- system.file("extdata", "AV069_SC.xml",
                 package = "MuscleTernary")
read_xfiber_xml(f)
#> # A tibble: 8,945 × 10
#>    track_num pt_pair x_origin y_origin z_origin x_insertion y_insertion
#>        <dbl> <chr>      <dbl>    <dbl>    <dbl>       <dbl>       <dbl>
#>  1         0 0,1       11273.   21712.   59873       11536.      21755.
#>  2         0 1,2       11536.   21755.   59935.      11767.      21791.
#>  3         0 2,3       11767.   21791.   59986.      11998.      21830.
#>  4         0 3,4       11998.   21830.   60040.      12227.      21868.
#>  5         0 4,5       12227.   21868.   60094.      12487.      21916.
#>  6         0 5,6       12487.   21916.   60161.      12787.      21983.
#>  7         0 6,7       12787.   21983.   60242.      13102.      22053.
#>  8         0 7,8       13102.   22053.   60327.      13434.      22141.
#>  9         0 8,9       13434.   22141.   60415.      13750.      22241.
#> 10         0 9,10      13750.   22241.   60504.      14082.      22371.
#> # ℹ 8,935 more rows
#> # ℹ 3 more variables: z_insertion <dbl>, OrientationTheta <dbl>,
#> #   OrientationPhi <dbl>
```
