test_that("batch_config returns matching files", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    c("file1.csv", "file2.csv")
  )
})

test_that("batch_config with no files", {
  path <- withr::local_tempdir()
  
  expect_error(
    batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    "^Directory '.*' does not contain any files matching '.*'[.]$"
  )
})

test_that("batch_config with no matching files", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 3), file.path(path, "file.csv"))

  expect_error(
    batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    "^Directory '.*' does not contain any files matching '.*'[.]$"
  )
})

test_that("batch_config with no path", {
  path <- withr::local_tempdir()
  unlink(path, recursive = TRUE)

  expect_error(batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    "^`path` must specify an existing directory [(]'.*' can't be found[)][.]$",
    class = "chk_error"
  )
})

test_that("batch_config with non-function", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))

  expect_error(batch_config(1, path = path, regexp = "^file\\d[.]csv$"),
    "^`fun` must be a function[.]$",
    class = "chk_error"
  )
})

test_that("batch_config recurse", {
  path <- withr::local_tempdir()
  sub <- withr::local_tempdir(tmpdir = path)

  write.csv(data.frame(x = 3), file.path(sub, "file3.csv"))

  expect_error(
    batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    "^Directory '.*' does not contain any files matching '.*'[.]$"
  )
  expect_identical(
    batch_config(function(x) TRUE,
      path = path, regexp = "^file\\d[.]csv$",
      recurse = TRUE
    ),
    file.path(basename(sub), "file3.csv")
  )
})

test_that("batch_config with existing .batchr.rds files", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 3), file.path(path, "file3.csv"))

  expect_identical(
    batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    "file3.csv"
  )
  expect_error(
    batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    "^Directory '.*' already contains a '[.]batchr[.]rds' file[.]$"
  )
})

test_that("batch_config with existing recursive .batchr.rds files", {
  path <- withr::local_tempdir()
  sub <- withr::local_tempdir(tmpdir = path)

  write.csv(data.frame(x = 3), file.path(sub, "file3.csv"))

  expect_identical(
    batch_config(function(x) TRUE,
      path = sub,
      regexp = "^file\\d[.]csv$"
    ),
    "file3.csv"
  )

  write.csv(data.frame(x = 2), file.path(path, "file2.csv"))

  expect_error(
    batch_config(function(x) TRUE,
      path = path, recurse = TRUE,
      regexp = "^file\\d[.]csv$"
    ),
    "^Subdirectories of '.*' contain '.batchr.rds' files[.]$"
  )

  expect_identical(
    batch_config(function(x) TRUE,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file2.csv"
  )
})
