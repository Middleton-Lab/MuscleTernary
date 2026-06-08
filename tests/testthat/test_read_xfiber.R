## Create minimal xfiber xlsx for testing #####################################

make_test_xlsx <- function() {
  skip_if_not_installed("writexl")
  nodes <- tibble::tibble(`Node ID` = 1L)
  points <- tibble::tibble(
    `Point ID` = c(1, 2, 3, 4),
    `X Coord` = c(1.0, 2.0, 3.0, 4.0),
    `Y Coord` = c(0.0, 1.0, 0.0, 1.0),
    `Z Coord` = c(0.0, 0.0, 1.0, 1.0)
  )
  segments <- tibble::tibble(
    `Segment ID` = c(0, 1),
    `Point IDs` = c("1,2", "3,4"),
    OrientationTheta = c(0.5, 0.6),
    OrientationPhi = c(0.3, 0.4)
  )
  tmpf <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    list(Nodes = nodes, Points = points, Segments = segments),
    tmpf
  )
  return(tmpf)
}

## read_xfiber #################################################################

test_that("read_xfiber reads xlsx and returns tibble", {
  tmpf <- make_test_xlsx()
  result <- read_xfiber(tmpf)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("track_num", "pt_pair",
                    "x_origin", "y_origin", "z_origin",
                    "x_insertion", "y_insertion", "z_insertion") %in%
                    names(result)))
  expect_gt(nrow(result), 0)
})

## write_segment ###############################################################

test_that("write_segment writes mel commands", {
  tmpf <- tempfile(fileext = ".mel")
  file.create(tmpf)
  write_segment(
    ID = "tr_1_1_2",
    x_origin = 1, y_origin = 0, z_origin = 0,
    x_insertion = 2, y_insertion = 1, z_insertion = 0,
    outfile = tmpf,
    radius = 4
  )
  content <- readLines(tmpf)
  expect_gt(length(content), 0)
  expect_true(any(grepl("curve", content, fixed = TRUE)))
})

## xfiber_to_maya ##############################################################

test_that("xfiber_to_maya writes mel file from xlsx", {
  tmpf_in <- make_test_xlsx()
  tmpf_out <- tempfile(fileext = ".mel")
  suppressMessages(xfiber_to_maya(tmpf_in, outfile = tmpf_out))
  expect_true(file.exists(tmpf_out))
  content <- readLines(tmpf_out)
  expect_gt(length(content), 0)
})

test_that("xfiber_to_maya with n=1 selects one track", {
  tmpf_in <- make_test_xlsx()
  tmpf_out <- tempfile(fileext = ".mel")
  suppressMessages(xfiber_to_maya(tmpf_in, outfile = tmpf_out, n = 1))
  expect_true(file.exists(tmpf_out))
})

test_that("xfiber_to_maya errors when n exceeds track count", {
  tmpf_in <- make_test_xlsx()
  tmpf_out <- tempfile(fileext = ".mel")
  expect_error(
    suppressMessages(xfiber_to_maya(tmpf_in, outfile = tmpf_out, n = 99))
  )
})
