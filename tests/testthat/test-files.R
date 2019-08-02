context("files")

test_that("batch_config_files", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config_files(path), character(0))
  expect_identical(batch_config(function(x) TRUE, path = path, pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  expect_identical(batch_config_files(path), ".batchr.rds")
})

test_that("batch_remaining_files errors if no configuration file", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 2), file.path(path, "file2.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file3.csv"))
  
  expect_error(batch_remaining_files(path), 
               "file '.*.batchr.rds' not found")
})

test_that("batch_remaining_files ignores later ones", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 2), file.path(path, "file2.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file3.csv"))
  
  expect_error(batch_remaining_files(path), 
               "file '.*.batchr.rds' not found")
  
  expect_identical(batch_config(function(x) TRUE, path = path, 
                                pattern = "^file\\d[.]csv$"),
                   c("file1.csv", "file2.csv", "file3.csv"))

  expect_identical(batch_remaining_files(path), 
                   c("file1.csv", "file2.csv", "file3.csv"))
  
  Sys.setFileTime(file.path(path, "file2.csv"), Sys.time())
  
  expect_identical(batch_remaining_files(path), 
                   c("file1.csv", "file3.csv"))
})

test_that("batch_remaining_files ignores non-matching ones", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file.csv"))
  write.csv(data.frame(x = 2), file.path(path, "file2.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file3.csv"))

  expect_error(batch_remaining_files(path), 
               "file '.*.batchr.rds' not found")
  
  expect_identical(batch_config(function(x) TRUE, path = path, 
                                pattern = "^file\\d[.]csv$"),
                   c("file2.csv", "file3.csv"))
  
  Sys.setFileTime(file.path(path, "file2.csv"), Sys.time())
  
  expect_identical(batch_remaining_files(path), 
                   "file3.csv")
})


test_that("batch_remaining_files gets failed ones", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 2), file.path(path, "file2.csv"))

  expect_error(batch_remaining_files(path), 
               "file '.*.batchr.rds' not found")
  
  expect_identical(batch_config(function(x) FALSE, path = path, 
                                pattern = "^file\\d[.]csv$"),
                   "file2.csv")
  
  expect_identical(batch_remaining_files(path), "file2.csv")
  expect_identical(batch_remaining_files(path ,failed = TRUE), character(0))
  expect_identical(batch_start(path), character(0))
  expect_identical(batch_remaining_files(path), character(0))
  expect_identical(batch_remaining_files(path, failed = TRUE),
                   "file2.csv")
})

test_that("batch_log_files", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_log_files(path), character(0))
  expect_identical(batch_config(function(x) FALSE, path = path, pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  expect_identical(batch_log_files(path), character(0))
  expect_identical(batch_start(path), character(0))
  expect_identical(batch_log_files(path), ".batchr.log")
})
