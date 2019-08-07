context("reconfig")

test_that("batch_reconfig_fun", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) FALSE, path = path, regexp = "^file\\d[.]csv$"),
                   c("file1.csv", "file2.csv"))
  expect_equal(batch_config_read(path)$fun, function(x) FALSE)
  batch_reconfig_fun(function(x) TRUE, path)
  expect_equal(batch_config_read(path)$fun, function(x) TRUE)
})

test_that("batch_config update with existing recursive .batchr.rds files", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 2), file.path(path, "file2.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, recurse = TRUE,
                               regexp = "^file\\d[.]csv$"),
               "file2.csv")
  
  path <- file.path(tempdir(), "batchr", "batchr_sub")
  dir.create(path)
  
  write.csv(data.frame(x = 3), file.path(path, "file3.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path,
                               regexp = "^file\\d[.]csv$"),
                   "file3.csv")
  
  path <- file.path(tempdir(), "batchr")

  expect_error(batch_reconfig_fun(function(x) 1, path),
               "^Subdirectories of '.*batchr' contain '[.]batchr[.]rds' files[.]$")

})

