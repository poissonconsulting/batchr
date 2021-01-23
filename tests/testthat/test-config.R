context("config")

test_that("batch_config returns matching files", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    c("file1.csv", "file2.csv")
  )
})

test_that("batch_config with no files", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)

  expect_error(
    batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    "^Directory '.*batchr' does not contain any files matching '.*'[.]$"
  )
})

test_that("batch_config with no matching files", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 3), file.path(path, "file.csv"))

  expect_error(
    batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    "^Directory '.*batchr' does not contain any files matching '.*'[.]$"
  )
})

test_that("batch_config with no path", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)

  expect_error(batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    "^`path` must specify an existing directory [(]'.*batchr' can't be found[)][.]$",
    class = "chk_error"
  )
})

test_that("batch_config with non-function", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))

  expect_error(batch_config(1, path = path, regexp = "^file\\d[.]csv$"),
    "^`fun` must be a function[.]$",
    class = "chk_error"
  )
})

test_that("batch_config recurse", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  path <- file.path(tempdir(), "batchr", "batchr_sub")
  dir.create(path, recursive = TRUE)

  write.csv(data.frame(x = 3), file.path(path, "file3.csv"))

  path <- file.path(tempdir(), "batchr")
  expect_error(
    batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    "^Directory '.*batchr' does not contain any files matching '.*'[.]$"
  )
  expect_identical(
    batch_config(function(x) TRUE,
      path = path, regexp = "^file\\d[.]csv$",
      recurse = TRUE
    ),
    "batchr_sub/file3.csv"
  )
})

test_that("batch_config with existing .batchr.rds files", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 3), file.path(path, "file3.csv"))

  expect_identical(
    batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    "file3.csv"
  )
  expect_error(
    batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    "^Directory '.*batchr' already contains a '[.]batchr[.]rds' file[.]$"
  )
})

test_that("batch_config with existing recursive .batchr.rds files", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)

  path <- file.path(tempdir(), "batchr", "batchr_sub")
  dir.create(path, recursive = TRUE)

  write.csv(data.frame(x = 3), file.path(path, "file3.csv"))

  expect_identical(
    batch_config(function(x) TRUE,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file3.csv"
  )

  path <- file.path(tempdir(), "batchr")

  write.csv(data.frame(x = 2), file.path(path, "file2.csv"))

  expect_error(
    batch_config(function(x) TRUE,
      path = path, recurse = TRUE,
      regexp = "^file\\d[.]csv$"
    ),
    "^Subdirectories of '.*batchr' contain '.batchr.rds' files[.]$"
  )

  expect_identical(
    batch_config(function(x) TRUE,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file2.csv"
  )
})
