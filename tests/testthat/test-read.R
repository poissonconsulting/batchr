context("read")

test_that("batch_read_config", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  config <- batch_read_config(path)
  
  expect_is(config$time, "POSIXct")
  expect_identical(attr(config$time, "tzone"), "UTC")
  config$time <- NULL
  
  expect_equal(config, 
                   list(pattern = "^file\\d[.]csv$", recursive = FALSE, 
                        FUN = function (x) 
TRUE, dots = list()))
})

test_that("batch_read_config with no setup", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_error(batch_read_config(path), 
               "^file '.*[.]batchr[.]rds' not found$")
})

test_that("batch_read_config with no directory", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)

  expect_error(batch_read_config(path), "^directory '.*batchr' not found$")
})
