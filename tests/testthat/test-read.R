test_that("batch_config_read", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    "file1.csv"
  )
  config <- batch_config_read(path)

  expect_is(config$time, "POSIXct")
  expect_identical(attr(config$time, "tzone"), "UTC")
  config$time <- NULL

  expect_identical(names(config), c("regexp", "recurse", "fun", "dots"))
  expect_identical(config$regexp, "^file\\d[.]csv$")
  expect_identical(config$recurse, FALSE)
  expect_is(config$fun, "function")
  expect_identical(config$fun(FALSE), TRUE)
  expect_identical(config$dots, list())
})

test_that("batch_config_read with no configuration", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_error(batch_config_read(path),
    "^Directory path [(]'.*'[)] must contain file '.batch.rds'[.]$",
    class = "chk_error"
  )
})

test_that("batch_config_read with no directory", {
  path <- withr::local_tempdir()
  unlink(path, recursive = TRUE)

  expect_error(batch_config_read(path),
    "^`path` must specify an existing directory [(]'.*' can't be found[)][.]$",
    class = "chk_error"
  )
})

test_that("batch_log_read not yet processed", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    "file1.csv"
  )
  
  log <- batch_log_read(path)
  expect_s3_class(log, "tbl_df")
  expect_identical(colnames(log), c("type", "time", "file", "message"))
  expect_identical(nrow(log), 0L)
  expect_is(log$type, "character")
  expect_is(log$time, "hms")
  expect_is(log$file, "character")
  expect_is(log$message, "character")
})

test_that("batch_log_read all processed successfully", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    "file1.csv"
  )

  log <- batch_log_read(path)
  expect_s3_class(log, "tbl_df")
  expect_identical(colnames(log), c("type", "time", "file", "message"))
  expect_identical(nrow(log), 0L)
  expect_is(log$type, "character")
  expect_is(log$time, "hms")
  expect_is(log$file, "character")
  expect_is(log$message, "character")

  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = TRUE))

  log <- batch_log_read(path)
  expect_s3_class(log, "tbl_df")
  expect_identical(colnames(log), c("type", "time", "file", "message"))
  expect_identical(nrow(log), 1L)
  expect_is(log$type, "character")
  expect_is(log$time, "hms")
  expect_is(log$file, "character")
  expect_is(log$message, "character")
  
  expect_identical(log$type, "SUCCESS")
  expect_lt(log$time, 5)
  expect_identical(log$file, "file1.csv")
  expect_identical(log$message, NA_character_)
})

test_that("batch_log_read 0.1 second", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) {
      Sys.sleep(0.101)
      TRUE
    }, path = path, regexp = "^file\\d[.]csv$"),
    "file1.csv"
  )

  log <- batch_log_read(path)
  expect_identical(nrow(log), 0L)

  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = TRUE))

  log <- batch_log_read(path)
  expect_identical(log$type, "SUCCESS")
  expect_gte(log$time, 0.09)
  expect_identical(log$file, "file1.csv")
})

test_that("batch_log_read all failed processing", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) FALSE, path = path, regexp = "^file\\d[.]csv$"),
    "file1.csv"
  )

  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = FALSE))

  log <- batch_log_read(path)
  expect_identical(nrow(log), 1L)
  expect_identical(log$type, "FAILURE")
  expect_identical(log$file, "file1.csv")
  expect_identical(log$message, NA_character_)
})

test_that("batch_log_read all error processing", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) { stop("a problem")  }, path = path, regexp = "^file\\d[.]csv$"),
    "file1.csv"
  )

  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = FALSE))

  log <- batch_log_read(path)
  expect_identical(log$type, "FAILURE")
  expect_identical(log$file, "file1.csv")
  expect_identical(log$message, "a problem")
})

test_that("batch_log_read one success (string) and one failure (error)", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file2.csv"))

  fun <- function(x) {
    if (grepl("file1[.]csv$", x)) {Sys.sleep(1e-05); stop("an error")}
    Sys.sleep(0.51)
    "a success"
  }

  expect_identical(
    batch_config(fun, path = path, regexp = "^file\\d[.]csv$"),
    c("file1.csv", "file2.csv")
  )

  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = FALSE, file2.csv = TRUE))

  log <- batch_log_read(path)
  log <- log[order(log$file),]
  
  expect_identical(log$type, c("FAILURE", "SUCCESS"))
  expect_gt(log$time[2], log$time[1])
  expect_identical(log$message, c("an error", "a success"))
})

test_that("batch_log_read with no configuration", {
  path <- withr::local_tempdir()
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_error(batch_log_read(path),
    "^Directory path [(]'.*'[)] must contain file '.batch.rds'[.]$",
    class = "chk_error"
  )
})

test_that("batch_log_read with no directory", {
  path <- withr::local_tempdir()
  unlink(path, recursive = TRUE)

  expect_error(batch_log_read(path),
    "^`path` must specify an existing directory [(]'.*' can't be found[)][.]$",
    class = "chk_error"
  )
})

test_that("batch_log_read parallel all processed successfully", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    "file1.csv"
  )

  log <- batch_log_read(path)
  expect_identical(nrow(log), 0L)

  options(mc.cores = 2)
  future::plan(future::multisession)
  teardown(future::plan(future::sequential))

  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = TRUE))

  log <- batch_log_read(path)
  expect_identical(nrow(log), 1L)
  expect_identical(log$type, "SUCCESS")
})

test_that("batch_log_read parallel one success (string) and one failure (error)", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file2.csv"))

  fun <- function(x) {
    if (grepl("file1[.]csv$", x)) {Sys.sleep(1e-05); stop("an error") }
    Sys.sleep(0.5)
    "a success"
  }

  expect_identical(
    batch_config(fun, path = path, regexp = "^file\\d[.]csv$"),
    c("file1.csv", "file2.csv")
  )

  options(mc.cores = 2)
  future::plan(future::multisession)
  teardown(future::plan(future::sequential))

  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = FALSE, file2.csv = TRUE))

  log <- batch_log_read(path)
  log <- log[order(log$file),]
  
  expect_identical(log$type, c("FAILURE", "SUCCESS"))
  expect_gt(log$time[2], log$time[1])
  expect_identical(log$message, c("an error", "a success"))
})
