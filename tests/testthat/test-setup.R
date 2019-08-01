context("setup")

test_that("batch_setup returns matching files", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_setup(identity, path = path, pattern = "^file\\d[.]csv$"),
                   c("file1.csv", "file2.csv"))
})

test_that("batch_setup with no files", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  expect_error(batch_setup(identity, path = path, pattern = "^file\\d[.]csv$"),
               "^no files match '")
})

test_that("batch_setup with no matching files", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 3), file.path(path, "file.csv"))
  
  expect_error(batch_setup(identity, path = path, pattern = "^file\\d[.]csv$"),
               "^no files match '")
})

test_that("batch_setup with no path", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  
  expect_error(batch_setup(identity, path = path, pattern = "^file\\d[.]csv$"),
               "batchr' does not exist")
})

test_that("batch_setup with non-function", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))

  expect_error(batch_setup(1, path = path, pattern = "^file\\d[.]csv$"),
                  "FUN must be a function")
})

test_that("batch_setup recursive", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  path <- file.path(tempdir(), "batchr", "batchr_sub")
  dir.create(path, recursive = TRUE)
  
  write.csv(data.frame(x = 3), file.path(path, "file3.csv"))
  
  path <- file.path(tempdir(), "batchr")
  expect_error(batch_setup(identity, path = path, pattern = "^file\\d[.]csv$"),
               "^no files match '")
  expect_identical(batch_setup(identity, path = path, pattern = "^file\\d[.]csv$",
                               recursive = TRUE),
                   "batchr_sub/file3.csv")
})

test_that("batch_setup with existing .batchr_setup.rds files", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 3), file.path(path, "file3.csv"))
  
  expect_identical(batch_setup(identity, path = path, pattern = "^file\\d[.]csv$"),
                   "file3.csv")
  expect_error(batch_setup(identity, path = path, pattern = "^file\\d[.]csv$"),
               "there are existing '.batchr_setup.rds' files")
})

test_that("batch_setup with existing recursive .batchr_setup.rds files", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  
  path <- file.path(tempdir(), "batchr", "batchr_sub")
  dir.create(path, recursive = TRUE)
  
  write.csv(data.frame(x = 3), file.path(path, "file3.csv"))
  
  path <- file.path(tempdir(), "batchr")
  
  expect_identical(batch_setup(identity, path = path, recursive = TRUE,
                               pattern = "^file\\d[.]csv$"),
                   "batchr_sub/file3.csv")
  expect_error(batch_setup(identity, path = path, pattern = "^file\\d[.]csv$"),
               "no files match '")
  expect_error(batch_setup(identity, path = path, pattern = "^file\\d[.]csv$",
                           recursive = TRUE),
               "there are existing '.batchr_setup.rds' files")
})
