context("files")

test_that("batch_files_remaining errors if no configuration file", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 2), file.path(path, "file2.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file3.csv"))

  expect_error(batch_files_remaining(path),
    "^Directory path [(]'.*'[)] must contain file '.batch.rds'[.]$",
    class = "chk_error"
  )
})

test_that("batch_files_remaining ignores later ones", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 2), file.path(path, "file2.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file3.csv"))

  expect_error(batch_files_remaining(path),
    "^Directory path [(]'.*'[)] must contain file '.batch.rds'[.]$",
    class = "chk_error"
  )

  expect_identical(
    batch_config(function(x) TRUE,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    c("file1.csv", "file2.csv", "file3.csv")
  )

  expect_identical(
    batch_files_remaining(path),
    c("file1.csv", "file2.csv", "file3.csv")
  )

  expect_identical(
    batch_files_remaining(path, failed = NA),
    c("file1.csv", "file2.csv", "file3.csv")
  )

  expect_identical(
    batch_files_remaining(path, failed = TRUE),
    character(0)
  )

  Sys.setFileTime(file.path(path, "file2.csv"), Sys.time())

  expect_identical(
    batch_files_remaining(path),
    c("file1.csv", "file3.csv")
  )

  expect_identical(
    batch_files_remaining(path),
    c("file1.csv", "file3.csv")
  )

  expect_identical(
    batch_files_remaining(path, failed = NA),
    c("file1.csv", "file3.csv")
  )

  expect_identical(
    batch_files_remaining(path, failed = TRUE),
    character(0)
  )
})

test_that("batch_files_remaining ignores non-matching ones", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file.csv"))
  write.csv(data.frame(x = 2), file.path(path, "file2.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file3.csv"))

  expect_error(batch_files_remaining(path),
    "^Directory path [(]'.*'[)] must contain file '.batch.rds'[.]$",
    class = "chk_error"
  )

  expect_identical(
    batch_config(function(x) TRUE,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    c("file2.csv", "file3.csv")
  )

  Sys.sleep(0.1)
  Sys.setFileTime(file.path(path, "file2.csv"), Sys.time())

  expect_identical(
    batch_files_remaining(path),
    "file3.csv"
  )
})

test_that("batch_files_remaining gets failed ones", {
  path <- withr::local_tempdir()
  
  write.csv(data.frame(x = 2), file.path(path, "file2.csv"))

  expect_error(batch_files_remaining(path),
    "^Directory path [(]'.*'[)] must contain file '.batch.rds'[.]$",
    class = "chk_error"
  )

  expect_identical(
    batch_config(function(x) FALSE,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file2.csv"
  )

  expect_identical(batch_files_remaining(path), "file2.csv")
  expect_identical(batch_files_remaining(path, failed = NA), "file2.csv")
  expect_identical(batch_files_remaining(path, failed = TRUE), character(0))
  expect_identical(batch_run(path, ask = FALSE), c(file2.csv = FALSE))
  expect_identical(batch_files_remaining(path), character(0))
  expect_identical(batch_files_remaining(path, failed = NA), "file2.csv")
  expect_identical(
    batch_files_remaining(path, failed = TRUE),
    "file2.csv"
  )
})

test_that("batch_files_remaining gets mix", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 2), file.path(path, "file2.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_error(batch_files_remaining(path),
    "^Directory path [(]'.*'[)] must contain file '.batch.rds'[.]$",
    class = "chk_error"
  )

  expect_identical(
    batch_config(function(x) grepl("file1[.]csv$", x),
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    c("file1.csv", "file2.csv")
  )

  expect_identical(batch_files_remaining(path), c("file1.csv", "file2.csv"))
  expect_identical(
    batch_files_remaining(path, failed = NA),
    c("file1.csv", "file2.csv")
  )
  expect_identical(batch_files_remaining(path, failed = TRUE), character(0))
  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = TRUE, file2.csv = FALSE))
  expect_identical(batch_files_remaining(path), character(0))
  expect_identical(
    batch_files_remaining(path, failed = NA),
    "file2.csv"
  )
  expect_identical(
    batch_files_remaining(path, failed = TRUE),
    "file2.csv"
  )
})
