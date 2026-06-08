## .validate_coord_cols (via coords_to_ternary) ################################

test_that("coords_to_ternary errors on missing muscle column", {
  df <- tibble::tibble(
    x_origin = 1, y_origin = 2, z_origin = 3,
    x_insertion = 4, y_insertion = 5, z_insertion = 6
  )
  expect_error(coords_to_ternary(df))
})

## .check_col_types (via coords_to_ternary) ####################################

test_that("coords_to_ternary errors on character non-grouping column", {
  df <- tibble::tibble(
    muscle = "A",
    label = "text",
    x_origin = 1, y_origin = 2, z_origin = 3,
    x_insertion = 4, y_insertion = 5, z_insertion = 6
  )
  expect_error(coords_to_ternary(df, grouping = "muscle"))
})

test_that("coords_to_ternary errors on factor non-grouping column", {
  df <- tibble::tibble(
    muscle = "A",
    category = factor("cat1"),
    x_origin = 1, y_origin = 2, z_origin = 3,
    x_insertion = 4, y_insertion = 5, z_insertion = 6
  )
  expect_error(coords_to_ternary(df, grouping = "muscle"))
})

## ends_to_vectors error paths #################################################

test_that("ends_to_vectors errors on missing columns", {
  df <- tibble::tibble(x_origin = 1, y_origin = 2, z_origin = 3)
  expect_error(ends_to_vectors(df))
})

## .check_mel_cols #############################################################

test_that("make_mel errors on missing required columns", {
  df <- tibble::tibble(muscle = "A", force = 100)
  expect_error(make_mel("test.stl", df))
})

## generate_shader #############################################################

test_that("generate_shader errors without muscle column", {
  shader <- tibble::tibble(R1 = 0.5, G1 = 0.5, B1 = 0.5)
  tmpf <- tempfile(fileext = ".mel")
  expect_error(generate_shader(shader, tmpf))
})

test_that("generate_shader writes valid mel output", {
  shader <- readr::read_csv(
    system.file("extdata", "muscle_colors.csv",
                package = "MuscleTernary"),
    show_col_types = FALSE
  )
  tmpf <- tempfile(fileext = ".mel")
  file.create(tmpf)
  generate_shader(shader, tmpf)
  content <- readLines(tmpf)
  expect_gt(length(content), 0)
  expect_true(any(grepl("shadingNode", content, fixed = TRUE)))
})
