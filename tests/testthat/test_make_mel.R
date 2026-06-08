## make_mel ####################################################################

test_that("make_mel writes a mel file", {
  dat <- readr::read_csv(
    system.file("extdata", "AL_008_data.csv",
                package = "MuscleTernary"),
    col_types = "ccddddddd"
  )
  tmpf <- tempfile(fileext = ".mel")
  suppressMessages(make_mel(
    stl = "L_dummy.stl",
    data = dat,
    outfile = tmpf,
    scale_radius = TRUE,
    use_stl = FALSE,
    write_file = TRUE
  ))
  expect_true(file.exists(tmpf))
  content <- readLines(tmpf)
  expect_gt(length(content), 0)
})

test_that("make_mel with scale_radius FALSE still works", {
  dat <- readr::read_csv(
    system.file("extdata", "AL_008_data.csv",
                package = "MuscleTernary"),
    col_types = "ccddddddd"
  )
  tmpf <- tempfile(fileext = ".mel")
  suppressMessages(make_mel(
    stl = "L_dummy.stl",
    data = dat,
    outfile = tmpf,
    scale_radius = FALSE,
    write_file = TRUE
  ))
  expect_true(file.exists(tmpf))
})

test_that("make_mel with write_file FALSE returns NULL", {
  dat <- readr::read_csv(
    system.file("extdata", "AL_008_data.csv",
                package = "MuscleTernary"),
    col_types = "ccddddddd"
  )
  result <- suppressMessages(make_mel(
    stl = "L_dummy.stl",
    data = dat,
    write_file = FALSE
  ))
  expect_null(result)
})

test_that("make_mel custom shader_file not csv errors", {
  dat <- readr::read_csv(
    system.file("extdata", "AL_008_data.csv",
                package = "MuscleTernary"),
    col_types = "ccddddddd"
  )
  expect_error(suppressMessages(make_mel(
    stl = "L_dummy.stl",
    data = dat,
    shader_file = "bad_file.txt",
    write_file = FALSE
  )))
})

test_that("make_mel with rev_arrows FALSE covers else branch", {
  dat <- readr::read_csv(
    system.file("extdata", "AL_008_data.csv",
                package = "MuscleTernary"),
    col_types = "ccddddddd"
  )
  tmpf <- tempfile(fileext = ".mel")
  suppressMessages(make_mel(
    stl = "L_dummy.stl",
    data = dat,
    outfile = tmpf,
    rev_arrows = FALSE,
    write_file = TRUE
  ))
  expect_true(file.exists(tmpf))
})

test_that("make_mel with use_stl TRUE covers centroid branch", {
  dat <- readr::read_csv(
    system.file("extdata", "AL_008_data.csv",
                package = "MuscleTernary"),
    col_types = "ccddddddd"
  )
  stl_path <- system.file("extdata", "L_mPTd_Or.stl",
                           package = "MuscleTernary")
  tmpf <- tempfile(fileext = ".mel")
  suppressMessages(make_mel(
    stl = stl_path,
    data = dat,
    outfile = tmpf,
    scale_radius = TRUE,
    use_stl = TRUE,
    write_file = TRUE
  ))
  expect_true(file.exists(tmpf))
})

test_that("make_mel with custom shader csv covers csv branch", {
  dat <- readr::read_csv(
    system.file("extdata", "AL_008_data.csv",
                package = "MuscleTernary"),
    col_types = "ccddddddd"
  )
  default_shader <- system.file("extdata", "muscle_colors.csv",
                                package = "MuscleTernary")
  tmp_shader <- tempfile(fileext = ".csv")
  file.copy(default_shader, tmp_shader)
  tmpf <- tempfile(fileext = ".mel")
  suppressMessages(make_mel(
    stl = "L_dummy.stl",
    data = dat,
    shader_file = tmp_shader,
    outfile = tmpf,
    write_file = TRUE
  ))
  expect_true(file.exists(tmpf))
})

test_that("make_mel with muscle not in shader errors", {
  dat <- tibble::tibble(
    muscle = "UNKNOWN_MUSCLE",
    side = "L",
    x_origin = 1, y_origin = 0, z_origin = 0,
    x_insertion = 2, y_insertion = 1, z_insertion = 0,
    force = 100
  )
  expect_error(suppressMessages(make_mel(
    stl = "L_dummy.stl",
    data = dat,
    write_file = FALSE
  )))
})
