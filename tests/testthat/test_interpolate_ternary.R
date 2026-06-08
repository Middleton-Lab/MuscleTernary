## interpolate_ternary #########################################################

test_that("interpolate_ternary returns a data.frame with correct rows", {
  row <- data.frame(x_1 = 0, y_1 = 0, z_1 = 0,
                    x_2 = 1, y_2 = 1, z_2 = 1)
  result <- interpolate_ternary(row, length_out = 5)
  expect_s3_class(result, "data.frame")
  expect_identical(nrow(result), 5L)
})

test_that("interpolate_ternary returns correct endpoint values", {
  row <- data.frame(x_1 = 0, y_1 = 0, z_1 = 0,
                    x_2 = 1, y_2 = 0, z_2 = 0)
  result <- interpolate_ternary(row, length_out = 3)
  expect_identical(result$x[1], 0)
  expect_identical(result$x[3], 1)
})

test_that("interpolate_ternary errors on multiple rows", {
  rows <- data.frame(x_1 = c(0, 1), y_1 = c(0, 0),
                     z_1 = c(0, 0), x_2 = c(1, 2),
                     y_2 = c(0, 0), z_2 = c(0, 0))
  expect_error(interpolate_ternary(rows))
})

test_that("interpolate_ternary errors on missing columns", {
  row <- data.frame(x_1 = 0, y_1 = 0, z_1 = 0)
  expect_error(interpolate_ternary(row))
})
