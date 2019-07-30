## AL008 example data #######################################################

M <- read_csv(system.file("extdata",
                     "AL_008_data.csv",
                     package = "MuscleTernary"),
              col_types = "ccddddddd") %>%
  dplyr::select(-side)

test_that("coords_to_ternary fails without necessary column names", {
  Msub <- M %>% select(dplyr::starts_with("x"))
  expect_error(coords_to_ternary(Msub))

  Msub <- M %>% dplyr::select(-muscle)
  expect_error(coords_to_ternary(Msub))
})

test_that("Row sum for x, y, z = 100", {
  D <- coords_to_ternary(M, grouping = "muscle") %>%
    select(x, y, z) %>%
    as.data.frame()
  expect_equal(rowSums(D), rep(100, times = nrow(D)))
})

## AL031 example data #######################################################

## Chicken example data #####################################################
