## MEL output snapshots ########################################################
# These lock the byte-for-byte MEL text emitted by the writers so that
# output-preserving refactors (e.g. paste0() -> glue()) are provably safe.
# Long decimals are truncated to four places so platform floating-point
# formatting cannot break the snapshot, and volatile lines (timestamps,
# absolute paths) are dropped before snapshotting.

mel_round <- function(lines) {
  gsub("(-?[0-9]+\\.[0-9]{4})[0-9]+", "\\1", lines)
}

test_that("write_arrows output is stable (rev_arrows = TRUE)", {
  tmpf <- tempfile(fileext = ".mel")
  file.create(tmpf)
  write_arrows(
    muscle = "mPTd", side = "L",
    x_origin = 10, y_origin = 20, z_origin = 30,
    x_insertion = 5, y_insertion = 15, z_insertion = 25,
    force = 100, cylinder_r = 2, cone_r = 4, cone_hr = 2,
    outfile = tmpf, rev_arrows = TRUE
  )
  expect_snapshot(writeLines(mel_round(readLines(tmpf))))
})

test_that("write_arrows output is stable (rev_arrows = FALSE)", {
  tmpf <- tempfile(fileext = ".mel")
  file.create(tmpf)
  write_arrows(
    muscle = "mPTd", side = "L",
    x_origin = 10, y_origin = 20, z_origin = 30,
    x_insertion = 5, y_insertion = 15, z_insertion = 25,
    force = 100, cylinder_r = 2, cone_r = 4, cone_hr = 2,
    outfile = tmpf, rev_arrows = FALSE
  )
  expect_snapshot(writeLines(mel_round(readLines(tmpf))))
})

test_that("write_segment output is stable", {
  tmpf <- tempfile(fileext = ".mel")
  file.create(tmpf)
  write_segment(
    ID = "tr_1_2",
    x_origin = 10, y_origin = 20, z_origin = 30,
    x_insertion = 5, y_insertion = 15, z_insertion = 25,
    outfile = tmpf, radius = 8
  )
  expect_snapshot(writeLines(mel_round(readLines(tmpf))))
})

test_that("generate_shader output is stable", {
  tmpf <- tempfile(fileext = ".mel")
  file.create(tmpf)
  shader <- tibble::tibble(
    muscle = c("mPTd", "mPTv"),
    R1 = c(0.10, 0.20), G1 = c(0.30, 0.40), B1 = c(0.50, 0.60)
  )
  generate_shader(shader, tmpf)
  expect_snapshot(writeLines(readLines(tmpf)))
})

test_that(".write_stl_import output is stable", {
  tmpf <- tempfile(fileext = ".mel")
  file.create(tmpf)
  .write_stl_import(tmpf, file_prefix = "model",
                    stl_path = "/models/model.stl")
  expect_snapshot(writeLines(readLines(tmpf)))
})

test_that(".write_mel_header note line is stable", {
  tmpf <- tempfile(fileext = ".mel")
  .write_mel_header(tmpf, data.frame(force = c(50, 100, 200)))
  lines <- readLines(tmpf)
  # Drop the absolute path and timestamp, which vary by run.
  lines <- lines[!grepl("^// (Generated|File):", lines)]
  expect_snapshot(writeLines(lines))
})
