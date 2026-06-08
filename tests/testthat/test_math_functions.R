## make_unit_vector ############################################################

test_that("make_unit_vector returns correct unit vector", {
  u <- make_unit_vector(c(3, 4, 0))
  expect_identical(u, c(0.6, 0.8, 0))
  expect_equal(sqrt(sum(u^2)), 1, tolerance = 1e-10)
})

test_that("make_unit_vector normalizes equal components", {
  u <- make_unit_vector(c(1, 1, 1))
  expect_equal(sqrt(sum(u^2)), 1, tolerance = 1e-10)
})

test_that("make_unit_vector errors on wrong length", {
  expect_error(make_unit_vector(c(1, 2)))
  expect_error(make_unit_vector(c(1, 2, 3, 4)))
})

## relative_proportion #########################################################

test_that("relative_proportion output sums to 1", {
  p <- relative_proportion(c(1, 2, 3))
  expect_equal(sum(p), 1, tolerance = 1e-10)
})

test_that("relative_proportion handles zero component", {
  p <- relative_proportion(c(3, 4, 0))
  expect_identical(p[3], 0)
  expect_equal(sum(p), 1, tolerance = 1e-10)
})

test_that("relative_proportion errors on wrong length", {
  expect_error(relative_proportion(c(1, 2)))
})

## xprod #######################################################################

test_that("xprod of orthogonal unit vectors is correct", {
  result <- xprod(c(1, 0, 0), c(0, 1, 0))
  expect_identical(result, c(0, 0, 1))
})

test_that("xprod of parallel vectors is zero", {
  result <- xprod(c(1, 0, 0), c(2, 0, 0))
  expect_identical(result, c(0, 0, 0))
})

test_that("xprod errors on wrong-length input", {
  expect_error(xprod(c(1, 2), c(1, 0, 0)))
  expect_error(xprod(c(1, 0, 0), c(1, 2)))
})

## dot #########################################################################

test_that("dot product of orthogonal vectors is zero", {
  expect_identical(dot(c(1, 0, 0), c(0, 1, 0)), 0)
})

test_that("dot product of parallel vectors is correct", {
  expect_identical(dot(c(1, 2, 3), c(4, 5, 6)), 32)
})

## muscle_force ################################################################

test_that("muscle_force uses default Tspec", {
  expect_identical(muscle_force(100), 30)
})

test_that("muscle_force uses custom Tspec", {
  expect_identical(muscle_force(500, Tspec = 0.25), 125)
})

## central_axis ################################################################

test_that("central_axis returns named unit vector", {
  ax <- central_axis(c(0, 0, 0), c(1, 1, 1))
  expect_named(ax, c("x", "y", "z"))
  expect_equal(sqrt(sum(ax^2)), 1, tolerance = 1e-10)
})

test_that("central_axis direction is correct", {
  ax <- central_axis(c(0, 0, 0), c(1, 0, 0))
  expect_equal(ax[["x"]], -1, tolerance = 1e-10)
  expect_equal(ax[["y"]], 0, tolerance = 1e-10)
  expect_equal(ax[["z"]], 0, tolerance = 1e-10)
})
