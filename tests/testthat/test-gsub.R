context("gsub")

test_that("batch_gsub", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  file <- file.path(path, "file.r")
  lines <- c("some zz", "and some more zzzz zzz zz z")
  writeLines(lines, file)
  file2 <- file.path(path, "file2.R")
  lines2 <- c("text")
  writeLines(lines2, file2)
  expect_error(
    batch_gsub(
      pattern = "zz", replacement = "z", path = path,
      regexp = "[.]txt$"
    ),
    "^Directory '.*batchr' does not contain any files matching '[[][.][]]txt[$]'[.]$"
  )
  expect_true(batch_gsub(
    pattern = "zz", replacement = "z", path = path,
    ask = FALSE
  ))

  expect_identical(readLines(file), c("some z", "and some more zz zz z z"))
  expect_identical(readLines(file2), c("text"))
  expect_true(batch_gsub(
    pattern = "zz", replacement = "z", path = path,
    ask = FALSE
  ))
  expect_identical(readLines(file), c("some z", "and some more z z z z"))
  expect_identical(readLines(file2), c("text"))
  expect_true(batch_gsub(
    pattern = "zz", replacement = "z", path = path,
    ask = FALSE
  ))
})
