# Find pennation angle

Measures pennation angles by calculating the angle between the central
axis and each fiber endpoint vector

## Usage

``` r
pennation_angle(vector_table, central_axis)
```

## Arguments

- vector_table::

  'data.frame' or 'tibble 'with unit vectors produced by
  'ends_to_vectors()'

- central:

  axis: Named numeric vector such as that produced by 'central_axis()'

## Value

Xfiber dataframe/tibble with newly appended "PennationAngle" column
