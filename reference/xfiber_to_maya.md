# Generate Maya mel file from xfiber tracing

Generate Maya mel file from xfiber tracing

## Usage

``` r
xfiber_to_maya(fname, outfile, radius = 8, n = NULL)
```

## Arguments

- fname:

  string: Path to excel file

- outfile:

  string: Path to output file

- radius:

  numeric: Radius of the cylinders in Maya

- n:

  numeric: Number of tracks to randomly select

## Value

Invisibly returns `NULL`. Called for its side effect of writing a Maya
mel script file.

## Examples

``` r
if (FALSE) { # \dontrun{
f <- system.file("extdata", "AV069_SC.xml",
                 package = "MuscleTernary")
xfiber_to_maya(f, outfile = tempfile(fileext = ".mel"))
} # }
```
