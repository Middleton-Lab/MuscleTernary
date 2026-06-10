# Write Maya arrows code

Write Maya arrows code

## Usage

``` r
write_arrows(
  muscle,
  side,
  x_origin,
  y_origin,
  z_origin,
  x_insertion,
  y_insertion,
  z_insertion,
  force,
  cylinder_r,
  cone_r,
  cone_hr,
  outfile,
  rev_arrows
)
```

## Arguments

- muscle:

  Muscle

- side:

  Side (L/R)

- x_origin:

  x origin

- y_origin:

  y origin

- z_origin:

  z origin

- x_insertion:

  x insertion

- y_insertion:

  y insertion

- z_insertion:

  z insertion

- force:

  Force (N)

- cylinder_r:

  Cylinder radius

- cone_r:

  Cone radius

- cone_hr:

  Cone height

- outfile:

  String file to write to

- rev_arrows:

  Boolean Reverse arrows or not

## Value

Invisibly returns `NULL`. Called for its side effect of writing MEL
commands to a file.

## Examples

``` r
if (FALSE) { # \dontrun{
write_arrows(
  muscle = "mPTd", side = "L",
  x_origin = 10, y_origin = 20, z_origin = 30,
  x_insertion = 5, y_insertion = 15, z_insertion = 25,
  force = 100, cylinder_r = 2, cone_r = 4, cone_hr = 2,
  outfile = tempfile(fileext = ".mel"), rev_arrows = TRUE
)
} # }
```
