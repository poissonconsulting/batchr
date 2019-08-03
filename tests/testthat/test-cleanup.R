context("cleanup")

test_that("batch_cleanup", {
  teardown(unlink(file.path(tempdir(), "batchr_start")))
  
  path <- file.path(tempdir(), "batchr_start")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, 
                                pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  expect_identical(batch_start(path), "file1.csv")
  expect_identical(batch_cleanup(path), ".")
  expect_identical(batch_cleanup(path), character(0))
})

test_that("batch_cleanup with all failed", {
  teardown(unlink(file.path(tempdir(), "batchr_start")))
  
  path <- file.path(tempdir(), "batchr_start")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) FALSE, path = path, 
                                pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  expect_identical(batch_start(path), character(0))
  expect_identical(batch_cleanup(path), character(0))
  expect_identical(batch_cleanup(path, force = TRUE), ".")
  expect_identical(batch_cleanup(path, force = TRUE), character(0))
})
