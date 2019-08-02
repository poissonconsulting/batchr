context("read")

test_that("batch_read_setup", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_setup(identity, path = path, pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  setup <- batch_read_setup(path)
  
  expect_is(setup$time, "POSIXct")
  expect_identical(attr(setup$time, "tzone"), "UTC")
  setup$time <- NULL
  
  expect_equal(setup, 
                   list(pattern = "^file\\d[.]csv$", recursive = FALSE, 
                        FUN = function (x) 
                          x, dots = list()))
})

test_that("read_setup with no setup", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_error(batch_read_setup(path), 
               "^file '.*[.]batchr_setup[.]rds' not found$")
})

test_that("read_setup with no directory", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)

  expect_error(batch_read_setup(path), "^directory '.*batchr' not found$")
})
