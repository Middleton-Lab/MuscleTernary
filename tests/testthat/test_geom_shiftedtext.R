## geom_shiftedtext ############################################################

test_that("geom_shiftedtext runs until ggtern namespace assignment", {
  expect_error(geom_shiftedtext(ggplot2::aes(label = "test")))
})
