# Estimate muscle force

Use PCSA and specific tension to estimate muscle force

## Usage

``` r
muscle_force(PCSA, Tspec = 0.3)
```

## Arguments

- PCSA:

  numeric: Value for PCSA (e.g., from
  [`pcsa()`](https://middleton-lab.github.io/MuscleTernary/reference/PCSA.md))

- Tspec:

  numeric: Value for specific tension of muscle in cubic mm. Default of
  0.3 is reasonable for mammalian muscle.

## Value

numeric: Estimate of muscle force
