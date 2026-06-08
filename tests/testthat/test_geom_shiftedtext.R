## geom_shiftedtext ############################################################

test_that("geom_shiftedtext returns a ggplot2 layer", {
  lyr <- geom_shiftedtext(ggplot2::aes(label = "test"))
  expect_s3_class(lyr, "LayerInstance")
})
