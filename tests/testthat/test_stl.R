stl_f <- system.file("extdata", "L_mPTd_Or.stl",
                     package = "MuscleTernary")
ins_f <- system.file("extdata", "L_mPTd_Ins.stl",
                     package = "MuscleTernary")

## read_stl ####################################################################

test_that("read_stl returns a matrix", {
  st <- read_stl(stl_f)
  expect_true(is.matrix(st))
  expect_identical(ncol(st), 3L)
  expect_gt(nrow(st), 0)
})

## centroid_location ###########################################################

test_that("centroid_location returns a 3-element numeric vector", {
  cl <- centroid_location(stl_f)
  expect_length(cl, 3)
  expect_type(cl, "double")
})

## centroid_size ###############################################################

test_that("centroid_size returns a positive scalar", {
  cs <- centroid_size(stl_f)
  expect_length(cs, 1)
  expect_gt(cs, 0)
})

## stl_area ####################################################################

test_that("stl_area returns a positive scalar", {
  area <- stl_area(stl_f)
  expect_length(area, 1)
  expect_gt(area, 0)
})

## pcsa ########################################################################

test_that("pcsa returns a positive scalar", {
  p <- suppressMessages(pcsa(stl_f, ins_f))
  expect_length(p, 1)
  expect_gt(p, 0)
})

test_that("pcsa with stl_area = FALSE also works", {
  p <- suppressMessages(pcsa(stl_f, ins_f, stl_area = FALSE))
  expect_length(p, 1)
  expect_gt(p, 0)
})

test_that("pcsa with units_adjust != 1 emits scaling message", {
  expect_message(
    pcsa(stl_f, ins_f, units_adjust = 1000),
    "Scaling by"
  )
})

test_that("read_stl error handler returns NA for nonexistent file", {
  result <- read_stl("/nonexistent/path/to/fake.stl")
  expect_true(is.na(result))
})
