test_that("batch_reconfig_fun", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) FALSE, path = path, regexp = "^file\\d[.]csv$"),
    c("file1.csv", "file2.csv")
  )
  expect_equal(batch_config_read(path)$fun, function(x) FALSE)
  batch_reconfig_fun(path, function(x) TRUE)
  expect_equal(batch_config_read(path)$fun, function(x) TRUE)
})

test_that("batch_config update with existing recursive .batchr.rds files", {
  path <- withr::local_tempdir()
  sub <- withr::local_tempdir(tmpdir = path)

  write.csv(data.frame(x = 2), file.path(path, "file2.csv"))

  expect_identical(
    batch_config(function(x) TRUE,
      path = path, recurse = TRUE,
      regexp = "^file\\d[.]csv$"
    ),
    "file2.csv"
  )

  write.csv(data.frame(x = 3), file.path(sub, "file3.csv"))

  expect_identical(
    batch_config(function(x) TRUE,
      path = sub,
      regexp = "^file\\d[.]csv$"
    ),
    "file3.csv"
  )

  expect_error(
    batch_reconfig_fun(path, function(x) 1),
    "^Subdirectories of '.*' contain '[.]batchr[.]rds' files[.]$"
  )
})

test_that("batch_reconfig_fileset update recurse with existing recursive .batchr.rds files", {
  path <- withr::local_tempdir()
  sub <- withr::local_tempdir(tmpdir = path)

  write.csv(data.frame(x = 3), file.path(sub, "file3.csv"))

  expect_identical(
    batch_config(function(x) TRUE,
      path = sub,
      regexp = "^file\\d[.]csv$"
    ),
    "file3.csv"
  )

  write.csv(data.frame(x = 2), file.path(path, "file.csv"))
  write.csv(data.frame(x = 2), file.path(path, "file2.csv"))

  expect_identical(
    batch_config(function(x) TRUE,
      path = path, recurse = FALSE,
      regexp = "^file\\d[.]csv$"
    ),
    "file2.csv"
  )

  expect_identical(batch_files_remaining(path), "file2.csv")

  expect_error(
    batch_reconfig_fileset(path, recurse = TRUE),
    "^Subdirectories of '.*' contain '[.]batchr[.]rds' files[.]$"
  )
})

test_that("batch_reconfig_fileset update regexp", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 2), file.path(path, "file.csv"))
  write.csv(data.frame(x = 2), file.path(path, "file2.csv"))

  expect_identical(
    batch_config(function(x) TRUE,
      path = path, recurse = FALSE,
      regexp = "^file\\d[.]csv$"
    ),
    "file2.csv"
  )

  expect_identical(batch_files_remaining(path), "file2.csv")

  expect_identical(
    batch_reconfig_fileset(path, regexp = "file\\d*[.]csv"),
    c("file.csv", "file2.csv")
  )
  expect_identical(
    batch_reconfig_fileset(path, regexp = "file[.]csv"),
    "file.csv"
  )
  expect_identical(
    batch_reconfig_fileset(path, regexp = "filet[.]csv"),
    character(0)
  )
})

test_that("batch_reconfig_fileset update neither", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 2), file.path(path, "file2.csv"))

  expect_identical(
    batch_config(function(x) TRUE,
      path = path, recurse = FALSE,
      regexp = "^file\\d[.]csv$"
    ),
    "file2.csv"
  )

  expect_error(
    batch_reconfig_fileset(path),
    "`regexp` and `recurse` must not both be NULL[.]$"
  )
})

test_that("batch_reconfig_fileset update recurse", {
  path <- withr::local_tempdir()
  sub <- withr::local_tempdir(tmpdir = path)

  write.csv(data.frame(x = 3), file.path(sub, "file3.csv"))

  write.csv(data.frame(x = 2), file.path(path, "file.csv"))
  write.csv(data.frame(x = 2), file.path(path, "file2.csv"))

  expect_identical(
    batch_config(function(x) TRUE,
      path = path, recurse = FALSE,
      regexp = "file\\d[.]csv$"
    ),
    "file2.csv"
  )

  expect_identical(batch_files_remaining(path), "file2.csv")

  expect_identical(
    sort(batch_reconfig_fileset(path, recurse = TRUE)),
    sort(c("file2.csv", file.path(basename(sub), "file3.csv")))
  )

  expect_identical(
    batch_reconfig_fileset(path, regexp = "file3[.]csv$"),
    file.path(basename(sub), "file3.csv")
  )

  expect_identical(
    batch_reconfig_fileset(path, regexp = basename(sub)),
    character(0)
  )
})
