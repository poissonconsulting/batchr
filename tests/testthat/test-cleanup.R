context("cleanup")

test_that("batch_cleanup", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(list.files(path), "file1.csv")
  
  expect_identical(
    batch_config(function(x) TRUE,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  expect_identical(batch_config_read(path)$regexp, "^file\\d[.]csv$")
  expect_identical(list.files(path), "file1.csv")
  expect_identical(list.files(path, "^file\\d[.]csv$"), "file1.csv")
  expect_identical(batch_files_remaining(path), "file1.csv")
  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = TRUE))
  expect_identical(batch_files_remaining(path, FALSE), character(0))
  expect_identical(batch_files_remaining(path, NA), character(0))
  expect_identical(batch_files_remaining(path, TRUE), character(0))
  expect_identical(batch_cleanup(path), c(. = TRUE))
  expect_identical(batch_cleanup(path), structure(logical(0), .Names = character(0)))
})

test_that("batch_cleanup with all failed", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(list.files(path), "file1.csv")
  
  expect_identical(
    batch_config(function(x) FALSE,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  expect_identical(batch_config_read(path)$regexp, "^file\\d[.]csv$")
  expect_identical(list.files(path), "file1.csv")
  expect_identical(list.files(path, "^file\\d[.]csv$"), "file1.csv")
  expect_identical(batch_files_remaining(path), "file1.csv")
  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = FALSE))
  expect_identical(batch_cleanup(path), c(. = FALSE))
  expect_identical(batch_cleanup(path, force = TRUE), c(. = TRUE))
  expect_identical(list.files(path, pattern = "^file\\d[.]csv$"), "file1.csv")
  expect_identical(
    batch_cleanup(path, force = TRUE),
    structure(logical(0), .Names = character(0))
  )
  expect_identical(
    batch_cleanup(path, force = TRUE),
    structure(logical(0), .Names = character(0))
  )
  expect_identical(
    batch_cleanup(path),
    structure(logical(0), .Names = character(0))
  )
})

test_that("batch_cleanup force remaining", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) FALSE,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  expect_identical(batch_config_read(path)$regexp, "^file\\d[.]csv$")
  expect_identical(list.files(path), "file1.csv")
  expect_identical(list.files(path, "^file\\d[.]csv$"), "file1.csv")
  expect_identical(batch_files_remaining(path), "file1.csv")
  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = FALSE))
  expect_identical(batch_cleanup(path), c(. = FALSE))
  expect_identical(batch_cleanup(path, force = TRUE, remaining = TRUE), c(. = TRUE))
  expect_identical(list.files(path, pattern = "^file\\d[.]csv$"), character(0))

  expect_identical(
    batch_cleanup(path, force = TRUE),
    structure(logical(0), .Names = character(0))
  )
  expect_identical(
    batch_cleanup(path, force = TRUE),
    structure(logical(0), .Names = character(0))
  )
  expect_identical(
    batch_cleanup(path),
    structure(logical(0), .Names = character(0))
  )
})
