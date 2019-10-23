context("status")

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

  expect_identical(batch_file_status(path),
                   structure(list(type = "REMAING", time = structure(NA_real_, class = c("hms", 
"difftime"), units = "secs"), file = "file1.csv", message = NA_character_), row.names = c(NA, 
-1L), class = c("tbl_df", "tbl", "data.frame")))
  
  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = TRUE))
  
  expect_identical(batch_file_status(path),
                   structure(list(type = "SUCCESS", time = structure(0, class = c("hms", 
"difftime"), units = "secs"), file = "file1.csv", message = NA_character_), row.names = c(NA, 
-1L), class = c("tbl_df", "tbl", "data.frame")))
})

test_that("batch_log_read all failed processing", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) stop("darn"), path = path, regexp = "^file\\d[.]csv$"),
    "file1.csv"
  )

  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = FALSE))

  expect_identical(batch_file_status(path),
                   structure(list(type = "FAILURE", time = structure(0, class = c("hms", 
"difftime"), units = "secs"), file = "file1.csv", message = "darn"), row.names = c(NA, 
-1L), class = c("tbl_df", "tbl", "data.frame")))
})
