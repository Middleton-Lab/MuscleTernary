# geom for labeling geom_points

See: http://stackoverflow.com/q/19694497/168137

## Usage

``` r
geom_shiftedtext(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  parse = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)
```

## Arguments

- mapping:

  The aes mapping

- data:

  The data

- stat:

  The stat

- position:

  The position

- parse:

  Parse or not

- na.rm:

  Remove NAs silently if TRUE

- show.legend:

  Whether to show in legend

- inherit.aes:

  Whether to inherit aesthetics

- ...:

  Additional options passed on

## Value

A geom usable by ggplot()

## Examples

``` r
if (FALSE) { # \dontrun{
geom_shiftedtext(ggplot2::aes(label = muscle))
} # }
```
