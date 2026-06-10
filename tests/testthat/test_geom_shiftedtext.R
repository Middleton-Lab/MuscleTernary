## geom_shiftedtext ############################################################

test_that("geom_shiftedtext returns a ggplot2 layer", {
  lyr <- geom_shiftedtext(ggplot2::aes(label = "test"))
  expect_s3_class(lyr, "LayerInstance")
})

test_that("GeomShiftedtext is a ggproto Geom extending GeomText", {
  expect_s3_class(GeomShiftedtext, "Geom")
  expect_s3_class(GeomShiftedtext, "GeomText")
})

test_that("geom_shiftedtext registers itself with ggtern", {
  geom_shiftedtext(ggplot2::aes(label = "test"))
  approved <- get(".approvedgeom", envir = asNamespace("ggtern"))
  expect_true("GeomShiftedtext" %in% approved)
})

test_that("draw_panel returns a null grob when no rows remain", {
  # All rows have a missing coordinate, so the geom should draw nothing
  # rather than error.
  grob <- GeomShiftedtext$draw_panel(
    tibble::tibble(x = NA_real_, y = 1, label = "a"),
    panel_params = NULL, coord = NULL
  )
  expect_s3_class(grob, "null")
})
