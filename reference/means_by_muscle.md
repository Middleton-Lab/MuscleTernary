# Means by Muscle

Aggregate means for each muscle.

## Usage

``` r
means_by_muscle(df_no_means)
```

## Arguments

- df_no_means:

  `data.frame` with two rows for each muscle

## Value

`data.frame` with means aggregated for each muscle.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  muscle = c("mPTd", "mPTd"),
  x = c(30, 40), y = c(35, 45), z = c(35, 15)
)
means_by_muscle(df)
} # }
```
