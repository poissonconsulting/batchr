context("read")

test_that("batch_config_read", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    "file1.csv"
  )
  config <- batch_config_read(path)

  expect_is(config$time, "POSIXct")
  expect_identical(attr(config$time, "tzone"), "UTC")
  config$time <- NULL

  expect_equal(
    config,
    list(
      regexp = "^file\\d[.]csv$", recurse = FALSE,
      fun = function(x)
        TRUE, dots = list()
    )
  )
})

test_that("batch_config_read with no configuration", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_error(batch_config_read(path),
    "^Directory path [(]'.*batchr'[)] must contain file '.batch.rds'[.]$",
    class = "chk_error"
  )
})

test_that("batch_config_read with no directory", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)

  expect_error(batch_config_read(path),
    "^`path` must specify an existing directory [(]'.*batchr' can't be found[)][.]$",
    class = "chk_error"
  )
})

test_that("batch_log_read not yet processed", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    "file1.csv"
  )

  expect_identical(
    batch_log_read(path),
    structure(list(type = character(0), time = structure(numeric(0), class = c(
      "POSIXct",
      "POSIXt"
    ), tzone = "UTC"), file = character(0), message = character(0)), class = c(
      "tbl_df",
      "tbl", "data.frame"
    ), row.names = integer(0))
  )
})

test_that("batch_log_read all processed successfully", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    "file1.csv"
  )

  expect_identical(
    batch_log_read(path),
    structure(list(type = character(0), time = structure(numeric(0), class = c(
      "POSIXct",
      "POSIXt"
    ), tzone = "UTC"), file = character(0), message = character(0)), class = c(
      "tbl_df",
      "tbl", "data.frame"
    ), row.names = integer(0))
  )

  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = TRUE))

  log <- batch_log_read(path)
  expect_identical(colnames(log), c("type", "time", "file", "message"))
  expect_s3_class(log$time, c("hms", "difftime"))
  expect_identical(round(as.numeric(log$time)), 0)

  expect_identical(log[c("type", "file")], structure(list(type = "SUCCESS", file = "file1.csv"), class = c(
    "tbl_df",
    "tbl", "data.frame"
  ), row.names = c(NA, -1L)))
})

test_that("batch_log_read 0.1 second", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) {Sys.sleep(0.101); TRUE}, path = path, regexp = "^file\\d[.]csv$"),
    "file1.csv"
  )

  expect_identical(
    batch_log_read(path),
    structure(list(type = character(0), time = structure(numeric(0), class = c(
      "POSIXct",
      "POSIXt"
    ), tzone = "UTC"), file = character(0), message = character(0)), class = c(
      "tbl_df",
      "tbl", "data.frame"
    ), row.names = integer(0))
  )

  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = TRUE))

  log <- batch_log_read(path)
  expect_identical(colnames(log), c("type", "time", "file", "message"))
  
  expect_s3_class(log$time, c("hms", "difftime"))
  expect_gte(log$time, 0.101)

  expect_identical(log[c("type", "file")], structure(list(type = "SUCCESS", file = "file1.csv"), class = c(
    "tbl_df",
    "tbl", "data.frame"
  ), row.names = c(NA, -1L)))
})


test_that("batch_log_read all failed processing", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) FALSE, path = path, regexp = "^file\\d[.]csv$"),
    "file1.csv"
  )

  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = FALSE))

  log <- batch_log_read(path)
  expect_identical(colnames(log), c("type", "time", "file", "message"))
  expect_s3_class(log$time, c("hms", "difftime"))
  expect_identical(round(as.numeric(log$time)), 0)
  expect_identical(log[c("type", "file", "message")], structure(list(type = "FAILURE", file = "file1.csv", message = NA_character_), class = c(
    "tbl_df",
    "tbl", "data.frame"
  ), row.names = c(NA, -1L)))
})

test_that("batch_log_read all error processing", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) stop("a problem"), path = path, regexp = "^file\\d[.]csv$"),
    "file1.csv"
  )

  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = FALSE))

  log <- batch_log_read(path)
  expect_identical(colnames(log), c("type", "time", "file", "message"))
  expect_s3_class(log$time, c("hms", "difftime"))
  expect_identical(round(as.numeric(log$time)), 0)
  expect_identical(log[c("type", "file")], structure(list(type = "FAILURE", file = "file1.csv"), class = c(
    "tbl_df",
    "tbl", "data.frame"
  ), row.names = c(NA, -1L)))
  expect_match(log$message, "^a problem$")
})


test_that("batch_log_read one success (string) and one failure (error)", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file2.csv"))

  fun <- function(x) {
    if (grepl("file1[.]csv$", x)) stop("an error")
    Sys.sleep(0.51)
    "a success"
  }

  expect_identical(
    batch_config(fun, path = path, regexp = "^file\\d[.]csv$"),
    c("file1.csv", "file2.csv")
  )

  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = FALSE, file2.csv = TRUE))

  log <- batch_log_read(path)
  expect_identical(colnames(log), c("type", "time", "file", "message"))
  
  expect_s3_class(log$time, c("hms", "difftime"))
  expect_identical(round(log$time), structure(c(0, 1), units = "secs", class = c("difftime")))
  
  expect_identical(log[c("type", "file")], structure(list(type = c("FAILURE", "SUCCESS"), file = c(
    "file1.csv",
    "file2.csv"
  )), class = c("tbl_df", "tbl", "data.frame"), row.names = c(
    NA,
    -2L
  )))
  expect_identical(log$message, c("an error", "a success"))
})

test_that("batch_log_read with no configuration", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_error(batch_log_read(path),
    "^Directory path [(]'.*batchr'[)] must contain file '.batch.rds'[.]$",
    class = "chk_error"
  )
})

test_that("batch_log_read with no directory", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)

  expect_error(batch_log_read(path),
    "^`path` must specify an existing directory [(]'.*batchr' can't be found[)][.]$",
    class = "chk_error"
  )
})

test_that("batch_log_read parallel all processed successfully", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    "file1.csv"
  )

  expect_identical(
    batch_log_read(path),
    structure(list(type = character(0), time = structure(numeric(0), class = c(
      "POSIXct",
      "POSIXt"
    ), tzone = "UTC"), file = character(0), message = character(0)), class = c(
      "tbl_df",
      "tbl", "data.frame"
    ), row.names = integer(0))
  )

  options(mc.cores = 2)
  future::plan(future::multisession)
  teardown(future::plan(future::sequential))
  
  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = TRUE))

  log <- batch_log_read(path)
  expect_identical(colnames(log), c("type", "time", "file", "message"))
  expect_s3_class(log$time, c("hms", "difftime"))
  expect_identical(round(as.numeric(log$time)), 0)

  expect_identical(log[c("type", "file")], structure(list(type = "SUCCESS", file = "file1.csv"), class = c(
    "tbl_df",
    "tbl", "data.frame"
  ), row.names = c(NA, -1L)))
})


test_that("batch_log_read parallel one success (string) and one failure (error)", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file2.csv"))

  fun <- function(x) {
    if (grepl("file1[.]csv$", x)) stop("an error")
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
  expect_identical(colnames(log), c("type", "time", "file", "message"))
  expect_s3_class(log$time, c("hms", "difftime"))
  expect_identical(as.numeric(round(log$time, 1)),
                   c(0, 0.5))

  expect_identical(log[c("type", "file")], structure(list(type = c("FAILURE", "SUCCESS"), file = c(
    "file1.csv",
    "file2.csv"
  )), class = c("tbl_df", "tbl", "data.frame"), row.names = c(
    NA,
    -2L
  )))
  expect_identical(log$message, c("an error", "a success"))
})

