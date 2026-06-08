xml_f <- system.file("extdata", "AV069_SC.xml",
                     package = "MuscleTernary")

## read_xfiber_xml #############################################################

test_that("read_xfiber_xml returns a tibble with expected columns", {
  D <- read_xfiber_xml(xml_f)
  expect_s3_class(D, "tbl_df")
  expect_true(all(c("track_num", "pt_pair",
                    "x_origin", "y_origin", "z_origin",
                    "x_insertion", "y_insertion", "z_insertion",
                    "OrientationTheta", "OrientationPhi") %in%
                    names(D)))
})

test_that("read_xfiber_xml returns numeric coordinate columns", {
  D <- read_xfiber_xml(xml_f)
  expect_type(D$x_origin, "double")
  expect_type(D$y_origin, "double")
  expect_type(D$z_origin, "double")
})

test_that("read_xfiber_xml returns positive number of rows", {
  D <- read_xfiber_xml(xml_f)
  expect_gt(nrow(D), 0)
})

## find_track_ends #############################################################

D_sc <- read_xfiber_xml(xml_f) |>
  dplyr::mutate(muscle = "SC")

test_that("find_track_ends returns one row per track", {
  D <- read_xfiber_xml(xml_f) |>
    dplyr::mutate(muscle = "SC")
  ends <- find_track_ends(D)
  expect_identical(nrow(ends), length(unique(D$track_num)))
})

test_that("find_track_ends preserves muscle column", {
  ends <- find_track_ends(D_sc)
  expect_true("muscle" %in% names(ends))
  expect_true(all(ends$muscle == "SC"))
})

test_that("find_track_ends errors without muscle column", {
  D_no_muscle <- read_xfiber_xml(xml_f)
  expect_error(find_track_ends(D_no_muscle))
})

## ends_to_vectors #############################################################

test_that("ends_to_vectors returns unit vectors", {
  ends <- find_track_ends(D_sc)
  vecs <- ends_to_vectors(ends)
  magnitudes <- sqrt(vecs$x^2 + vecs$y^2 + vecs$z^2)
  expect_true(all(abs(magnitudes - 1) < 1e-8))
})

test_that("ends_to_vectors has correct columns", {
  ends <- find_track_ends(D_sc)
  vecs <- ends_to_vectors(ends)
  expect_true(all(c("muscle", "x", "y", "z") %in% names(vecs)))
})

test_that("ends_to_vectors with grouping returns one row per muscle", {
  ends <- find_track_ends(D_sc)
  vecs <- ends_to_vectors(ends, grouping = "muscle")
  expect_identical(nrow(vecs), 1L)
})
