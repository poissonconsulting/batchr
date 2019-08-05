context("process")

test_that("batch_process",{
  teardown(unlink(file.path(tempdir(), "batchr_process")))
  
  path <- file.path(tempdir(), "batchr_process")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_true(batch_process(function(x) TRUE, path = path, 
                            pattern = "^file\\d[.]csv$"))
})

test_that("batch_process changes files", {
  teardown(unlink(file.path(tempdir(), "batchr_process")))
  
  path <- file.path(tempdir(), "batchr_process")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))
  
  FUN <- function(file) {
    data <- read.csv(file)
    data$x <- data$x * 2
    write.csv(data, file)
    TRUE
  }

  expect_true(batch_process(FUN, path))
  # should be x = 2L
  expect_identical(read.csv(file.path(path, "file1.csv")), 
                   structure(list(X.1 = 1L, X = 1L, x = 2L), class = "data.frame", row.names = c(NA, -1L)))
  # should be x = 6L
  expect_identical(read.csv(file.path(path, "file2.csv")), 
                   structure(list(X.1 = 1L, X = 1L, x = 6L), class = "data.frame", row.names = c(NA, -1L)))
})

test_that("batch_process with failure", {
  teardown(unlink(file.path(tempdir(), "batchr_process")))
  
  path <- file.path(tempdir(), "batchr_process")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))
  
  FUN <- function(file) grepl(file, "file1[.]csv$")

  expect_error(batch_read_config(path),
               "^Can't find file '.*[.]batchr.rds'[.]$")
  expect_false(batch_process(FUN, path))
  expect_error(batch_read_config(path),
               "^Can't find file '.*[.]batchr.rds'[.]$")
})
