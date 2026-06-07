# Calculate the Euler angles for rotating A onto B.

The utility is to set up geometry with a known normal (0, 1, 0), and
calculate the xyz rotation sequence to match another vector.

## Usage

``` r
get_euler_angles(A, B)
```

## Arguments

- A:

  Vector (usually the unit vector)

- B:

  Vector

## Value

Euler angles between A and B
