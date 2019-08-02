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

test_that("batch_read_config with no configuration", {
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

test_that("batch_read_log not yet processed", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  
  expect_identical(batch_read_log(path),
                   structure(list(level = structure(integer(0), .Label = c("DEBUG", 
"INFO", "WARN", "ERROR", "FATAL"), class = c("ordered", "factor"
)), time = structure(numeric(0), class = c("POSIXct", "POSIXt"
), tzone = "UTC"), file = character(0)), class = c("tbl_df", 
"tbl", "data.frame"), row.names = integer(0)))
})

test_that("batch_read_log all processed successfully", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  
  expect_identical(batch_start(path), "file1.csv")
  
  expect_identical(batch_read_log(path),
                   structure(list(level = structure(integer(0), .Label = c("DEBUG", 
"INFO", "WARN", "ERROR", "FATAL"), class = c("ordered", "factor"
)), time = structure(numeric(0), class = c("POSIXct", "POSIXt"
), tzone = "UTC"), file = character(0)), class = c("tbl_df", 
"tbl", "data.frame"), row.names = integer(0)))
})

test_that("batch_read_log all failed processing", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) FALSE, path = path, pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  
  expect_identical(batch_start(path), character(0))
  
  log <- batch_read_log(path)
  expect_identical(colnames(log), c("level", "time", "file"))
  expect_is(log$time, "POSIXct")
  expect_identical(attr(log$time, "tzone"), "UTC")
  expect_identical(log[c("level", "file")], structure(list(level = structure(2L, .Label = c("DEBUG", "INFO", 
"WARN", "ERROR", "FATAL"), class = c("ordered", "factor")), file = "file1.csv"), class = c("tbl_df", 
"tbl", "data.frame"), row.names = c(NA, -1L)))
})

test_that("batch_read_log all error processing", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) stop("a problem"), path = path, pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  
  expect_identical(batch_start(path), character(0))
  
  log <- batch_read_log(path)
  expect_identical(colnames(log), c("level", "time", "file"))
  expect_is(log$time, "POSIXct")
  expect_identical(attr(log$time, "tzone"), "UTC")
  expect_identical(log[c("level", "file")], structure(list(level = structure(4L, .Label = c("DEBUG", "INFO", 
"WARN", "ERROR", "FATAL"), class = c("ordered", "factor")), file = "file1.csv"), class = c("tbl_df", 
"tbl", "data.frame"), row.names = c(NA, -1L)))
})

test_that("batch_read_log all fatal error processing", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) 1, path = path, pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  
  expect_error(batch_start(path), "processing file 'file1.csv' returned an object of class 'numeric'")
  
  log <- batch_read_log(path)
  expect_identical(colnames(log), c("level", "time", "file"))
  expect_is(log$time, "POSIXct")
  expect_identical(attr(log$time, "tzone"), "UTC")
  expect_identical(log[c("level", "file")], structure(list(level = structure(5L, .Label = c("DEBUG", "INFO", 
"WARN", "ERROR", "FATAL"), class = c("ordered", "factor")), file = "file1.csv"), class = c("tbl_df", 
"tbl", "data.frame"), row.names = c(NA, -1L)))
})

test_that("batch_read_log with no configuration", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_error(batch_read_log(path), 
               "^file '.*[.]batchr[.]rds' not found$")
})

test_that("batch_read_log with no directory", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)

  expect_error(batch_read_log(path), "^directory '.*batchr' not found$")
})