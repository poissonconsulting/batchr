context("status")

test_that("batch_file_status all processed successfully", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    "file1.csv"
  )

  status <- batch_file_status(path)
  expect_identical(status,
    structure(list(type = "REMAING", time = structure(NA_real_, class = c("hms",
      "difftime"), units = "secs"), file = "file1.csv", message = NA_character_), row.names = c(NA,
      -1L), class = c("tbl_df", "tbl", "data.frame")))

  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = TRUE))

  status <- batch_file_status(path)
  expect_identical(colnames(status), c("type", "time", "file", "message"))

  expect_identical(status[c("type", "file", "message")],
    structure(list(type = "SUCCESS", file = "file1.csv", message = NA_character_), row.names = c(NA,
      -1L), class = c("tbl_df", "tbl", "data.frame")))

  expect_identical(round(as.numeric(status$time)), 0)
})

test_that("batch_file_status all failed processing", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) {Sys.sleep(1e-05); stop("darn")}, path = path, regexp = "^file\\d[.]csv$"),
    "file1.csv"
  )

  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = FALSE))

  status <- batch_file_status(path)
  expect_identical(colnames(status), c("type", "time", "file", "message"))
  expect_identical(status[c("type", "file", "message")],
    structure(list(type = "FAILURE", file = "file1.csv", message = "darn"), row.names = c(NA,
      -1L), class = c("tbl_df", "tbl", "data.frame")))
  expect_identical(round(as.numeric(status$time)), 0)
})
