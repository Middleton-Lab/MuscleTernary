## means_by_muscle #############################################################

test_that("means_by_muscle returns one row per muscle", {
  df <- tibble::tibble(
    muscle = c("A", "A", "B", "B"),
    x = c(10, 20, 30, 40),
    y = c(1, 3, 5, 7)
  )
  result <- suppressMessages(means_by_muscle(df))
  expect_identical(nrow(result), 2L)
})

test_that("means_by_muscle computes correct means", {
  df <- tibble::tibble(
    muscle = c("A", "A"),
    x = c(10, 20),
    y = c(4, 8)
  )
  result <- suppressMessages(means_by_muscle(df))
  expect_identical(result$x[result$muscle == "A"], 15)
  expect_identical(result$y[result$muscle == "A"], 6)
})

test_that("means_by_muscle errors on wrong row count", {
  df <- tibble::tibble(
    muscle = c("A", "A", "A"),
    x = c(10, 20, 30)
  )
  expect_error(suppressMessages(means_by_muscle(df)))
})

test_that("means_by_muscle handles factor columns", {
  df <- data.frame(
    muscle = c("A", "A"),
    x = c(10, 20),
    side = factor(c("L", "R")),
    stringsAsFactors = FALSE
  )
  result <- suppressMessages(means_by_muscle(df))
  expect_identical(nrow(result), 1L)
  expect_true("muscle" %in% names(result))
})

## muscle_color_map ############################################################

test_that("muscle_color_map returns a ScaleDiscrete", {
  cmap <- muscle_color_map()
  expect_s3_class(cmap, "ScaleDiscrete")
})

## muscle_fill_map #############################################################

test_that("muscle_fill_map returns a ScaleDiscrete", {
  fmap <- muscle_fill_map()
  expect_s3_class(fmap, "ScaleDiscrete")
})
