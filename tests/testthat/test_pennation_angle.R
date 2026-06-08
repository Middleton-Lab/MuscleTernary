xml_f <- system.file("extdata", "AV069_SC.xml",
                     package = "MuscleTernary")

test_that("pennation_angle appends PennationAngle column", {
  D <- read_xfiber_xml(xml_f) |>
    dplyr::mutate(muscle = "SC")
  ends <- find_track_ends(D)
  vecs <- ends_to_vectors(ends)
  ax <- central_axis(c(0, 0, 0), c(1, 1, 1))
  result <- pennation_angle(vecs, ax)
  expect_true("PennationAngle" %in% names(result))
})

test_that("pennation_angle values are between 0 and 90 degrees", {
  D <- read_xfiber_xml(xml_f) |>
    dplyr::mutate(muscle = "SC")
  ends <- find_track_ends(D)
  vecs <- ends_to_vectors(ends)
  ax <- central_axis(c(0, 0, 0), c(1, 1, 1))
  result <- pennation_angle(vecs, ax)
  expect_true(all(result$PennationAngle >= 0))
  expect_true(all(result$PennationAngle <= 90))
})
