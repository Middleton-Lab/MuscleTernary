test_that("muscle_color_map() should be a function", {
  expect_type(muscle_color_map, "closure")
})

test_that("muscle_fill_map() should be a function", {
  expect_type(muscle_fill_map, "closure")
})
