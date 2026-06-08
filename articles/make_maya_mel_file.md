# Make Maya mel File

Generating a Maya `.mel` script from a dataset is fairly
straightforward. If the data is already in the format for making a
ternary plot (including `muscle` and the `orogin` and `insertion`
columns), then it is as simple as calling
[`make_mel()`](https://middleton-lab.github.io/MuscleTernary/reference/make_mel.md).
If the data are coming from stl mesh files, then follow the steps in
“Working with stl files” to make a tibble ready for ternary plotting.
This would replace `AL_008` int he code below.

``` r

library(MuscleTernary)
library(readr)

AL008 <- read_csv(system.file("extdata",
                             "AL_008_data.csv",
                             package = "MuscleTernary"),
                 show_col_types = FALSE)
AL008
#> # A tibble: 18 × 9
#>    muscle side  x_origin y_origin z_origin x_insertion y_insertion z_insertion
#>    <chr>  <chr>    <dbl>    <dbl>    <dbl>       <dbl>       <dbl>       <dbl>
#>  1 mAMES  L         87.6     41.6   -168.        109.         4.92      -133. 
#>  2 mAMES  R        -83.8     45.1   -163.       -108.         7.69      -141. 
#>  3 mAMEM  L         34.2     42.8   -140.        103.        -1.59      -129. 
#>  4 mAMEM  R        -30.2     43.4   -139.       -103.         2.88      -144. 
#>  5 mAMEP  L         27.1     65.4   -130.         92.2       -9.95       -71.7
#>  6 mAMEP  R        -19.7     69.6   -128.        -90.2       -2.99       -79.1
#>  7 mAMP   L         64.2     35.9   -172.        110.       -53.8       -140. 
#>  8 mAMP   R        -63.6     35.8   -173.       -108.       -51.9       -138. 
#>  9 mPSTs  L         23.3     68.3   -118.         95.9      -78.9        -17.5
#> 10 mPSTs  R        -16.9     69.0   -116.        -88.2      -78.5        -11.9
#> 11 mPSTp  L         19.5     48.9   -119.         97.8      -59.6       -143. 
#> 12 mPSTp  R        -12.5     48.7   -117.        -92.9      -58.1       -144. 
#> 13 mPTd   L         38.8     21.3    -68.8        91.0      -28.5       -222. 
#> 14 mPTd   R        -34.9     20.6    -62.6       -91.5      -26.2       -221. 
#> 15 mPTv   L         47.8    -50.5   -129.        112.       -21.2       -246. 
#> 16 mPTv   R        -40.9    -47.4   -129.       -114.       -22.1       -239. 
#> 17 mDM    L         76.4     56.2   -206.         86.7        9.90      -255. 
#> 18 mDM    R        -75.5     58.0   -201.        -90.7       13.1       -251. 
#> # ℹ 1 more variable: force <dbl>
```

We’ll pass the parameter `write_file = FALSE` so that the `.mel` file is
not written to disk. In normal practice, you would set
`write_file = TRUE` (or leave out this option because this is the
default behavior).

``` r

make_mel(stl = "AL_008.stl",
         shader_file = "default",
         data = AL008,
         write_file = FALSE)
```

Once the file is written, move it to the same folder as your stl mesh
(if it is not already) and execute in Maya. This script contains all the
information that Maya needs to import the stl, create the arrows, scale
and color them.
