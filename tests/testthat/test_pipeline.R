## End-to-end pipeline #########################################################
# Exercises the documented Xfiber workflow from a raw XML file through to
# ternary-ready coordinates, checking the mathematical properties that should
# hold at each stage.

test_that("xfiber XML flows through to unit vectors and ternary coords", {
  xml_f <- system.file("extdata", "AV069_SC.xml",
                       package = "MuscleTernary")
  D <- read_xfiber_xml(xml_f) |>
    dplyr::mutate(muscle = "SC")
  ends <- find_track_ends(D)

  # One row per track after reduction.
  expect_identical(nrow(ends), length(unique(D$track_num)))

  # ends_to_vectors() produces unit vectors: x^2 + y^2 + z^2 == 1.
  vecs <- ends_to_vectors(ends)
  expect_true(
    all(abs(rowSums(vecs[, c("x", "y", "z")]^2) - 1) < 1e-8)
  )

  # coords_to_ternary() produces proportions that sum to 100 per row.
  tern <- suppressMessages(
    coords_to_ternary(ends, grouping = "muscle")
  )
  expect_s3_class(tern, "data.frame")
  expect_true(all(c("x", "y", "z") %in% names(tern)))
  expect_true(
    all(abs(rowSums(tern[, c("x", "y", "z")]) - 100) < 1e-6)
  )
})
