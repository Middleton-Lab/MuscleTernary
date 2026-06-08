# Import from Avizo xfiber Excel

By default, xfiber write an excel file in XML format. Loading such a
file and the resaving as xlsx will allow it to be read into R with this
function.

## Usage

``` r
read_xfiber(filename)
```

## Arguments

- filename:

  string: file name to load (should be xlsx).

## Value

tibble: All tracks concatenated.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires an xlsx file produced by Avizo xfiber
f <- system.file("extdata", "myfibers.xlsx",
                 package = "MuscleTernary")
read_xfiber(f)
} # }
```
