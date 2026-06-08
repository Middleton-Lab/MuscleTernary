## write_arrows ################################################################

test_that("write_arrows writes mel commands to file", {
  tmpf <- tempfile(fileext = ".mel")
  file.create(tmpf)
  write_arrows(
    muscle = "mPTd", side = "L",
    x_origin = 10, y_origin = 20, z_origin = 30,
    x_insertion = 5, y_insertion = 15, z_insertion = 25,
    force = 100, cylinder_r = 2, cone_r = 4, cone_hr = 2,
    outfile = tmpf, rev_arrows = TRUE
  )
  content <- readLines(tmpf)
  expect_gt(length(content), 0)
  expect_true(any(grepl("mPTd_L", content, fixed = TRUE)))
  expect_true(any(grepl("curve", content, fixed = TRUE)))
})

test_that("write_arrows with rev_arrows FALSE reverses coords", {
  tmpf1 <- tempfile(fileext = ".mel")
  tmpf2 <- tempfile(fileext = ".mel")
  file.create(tmpf1)
  file.create(tmpf2)
  write_arrows(
    muscle = "mPTd", side = "L",
    x_origin = 10, y_origin = 20, z_origin = 30,
    x_insertion = 5, y_insertion = 15, z_insertion = 25,
    force = 100, cylinder_r = 2, cone_r = 4, cone_hr = 2,
    outfile = tmpf1, rev_arrows = TRUE
  )
  write_arrows(
    muscle = "mPTd", side = "L",
    x_origin = 10, y_origin = 20, z_origin = 30,
    x_insertion = 5, y_insertion = 15, z_insertion = 25,
    force = 100, cylinder_r = 2, cone_r = 4, cone_hr = 2,
    outfile = tmpf2, rev_arrows = FALSE
  )
  content1 <- readLines(tmpf1)
  content2 <- readLines(tmpf2)
  expect_false(identical(content1, content2))
})

test_that("write_arrows returns NULL invisibly", {
  tmpf <- tempfile(fileext = ".mel")
  file.create(tmpf)
  result <- write_arrows(
    muscle = "mPTd", side = "L",
    x_origin = 10, y_origin = 20, z_origin = 30,
    x_insertion = 5, y_insertion = 15, z_insertion = 25,
    force = 100, cylinder_r = 2, cone_r = 4, cone_hr = 2,
    outfile = tmpf, rev_arrows = TRUE
  )
  expect_null(result)
})
