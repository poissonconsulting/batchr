context("config")

test_that("batch_config returns matching files", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, pattern = "^file\\d[.]csv$"),
                   c("file1.csv", "file2.csv"))
})

test_that("batch_reconfig", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) FALSE, path = path, pattern = "^file\\d[.]csv$"),
                   c("file1.csv", "file2.csv"))
  expect_equal(batch_read_config(path)$FUN, function(x) FALSE)
  batch_reconfig(function(x) TRUE, path)
  expect_equal(batch_read_config(path)$FUN, function(x) TRUE)
})

test_that("batch_config with no files", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  expect_error(batch_config(function(x) TRUE, path = path, pattern = "^file\\d[.]csv$"),
               "^Directory '.*batchr' does not contain any files matching '.*'[.]$")
})

test_that("batch_config with no matching files", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 3), file.path(path, "file.csv"))
  
  expect_error(batch_config(function(x) TRUE, path = path, pattern = "^file\\d[.]csv$"),
               "^Directory '.*batchr' does not contain any files matching '.*'[.]$")
})

test_that("batch_config with no path", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  
  expect_error(batch_config(function(x) TRUE, path = path, pattern = "^file\\d[.]csv$"),
               "^Can't find directory '.*batchr'[.]$")
})

test_that("batch_config with non-function", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))
  
  expect_error(batch_config(1, path = path, pattern = "^file\\d[.]csv$"),
               "^`FUN` must be a function[.]$")
})

test_that("batch_config recursive", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  path <- file.path(tempdir(), "batchr", "batchr_sub")
  dir.create(path, recursive = TRUE)
  
  write.csv(data.frame(x = 3), file.path(path, "file3.csv"))
  
  path <- file.path(tempdir(), "batchr")
  expect_error(batch_config(function(x) TRUE, path = path, pattern = "^file\\d[.]csv$"),
               "^Directory '.*batchr' does not contain any files matching '.*'[.]$")
  expect_identical(batch_config(function(x) TRUE, path = path, pattern = "^file\\d[.]csv$",
                               recursive = TRUE),
                   "batchr_sub/file3.csv")
})

test_that("batch_config with existing .batchr.rds files", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 3), file.path(path, "file3.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, pattern = "^file\\d[.]csv$"),
                   "file3.csv")
  expect_error(batch_config(function(x) TRUE, path = path, pattern = "^file\\d[.]csv$"),
               "^Directory '.*batchr' already contains a '[.]batchr[.]rds' file[.]$")
})

test_that("batch_config with existing recursive .batchr.rds files", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  
  path <- file.path(tempdir(), "batchr", "batchr_sub")
  dir.create(path, recursive = TRUE)
  
  write.csv(data.frame(x = 3), file.path(path, "file3.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path,
                               pattern = "^file\\d[.]csv$"),
                   "file3.csv")
  
  path <- file.path(tempdir(), "batchr")
  
  write.csv(data.frame(x = 2), file.path(path, "file2.csv"))
  
  expect_error(batch_config(function(x) TRUE, path = path, recursive = TRUE,
                               pattern = "^file\\d[.]csv$"),
               "^Subdirectories of '.*batchr' contain '.batchr.rds' files[.]$")

  expect_identical(batch_config(function(x) TRUE, path = path,
                               pattern = "^file\\d[.]csv$"),
                   "file2.csv")
})
