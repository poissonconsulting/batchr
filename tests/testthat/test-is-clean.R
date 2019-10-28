context("is-clean")

test_that("batch_is_clean()",{
  teardown(unlink(file.path(tempdir(), "batchr_start")))

  path <- file.path(tempdir(), "batchr_start")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_true(batch_is_clean(path))

  expect_identical(
    batch_config(function(x) TRUE,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  expect_false(batch_is_clean(path))
  
  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = TRUE))
  expect_false(batch_is_clean(path))

  expect_identical(batch_cleanup(path), c(. = TRUE))
  expect_true(batch_is_clean(path))
})

test_that("batch_is_clean() recurse",{
  teardown(unlink(file.path(tempdir(), "batchr_start")))

  path <- file.path(tempdir(), "batchr_start/sub")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_true(batch_is_clean(path))
  expect_true(batch_is_clean(dirname(path)))
  expect_true(batch_is_clean(dirname(path), recurse = TRUE))

  expect_identical(
    batch_config(function(x) TRUE,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  expect_false(batch_is_clean(path))
  expect_true(batch_is_clean(dirname(path)))
  expect_false(batch_is_clean(dirname(path), recurse = TRUE))

  expect_identical(batch_cleanup(path, force = TRUE), c(. = TRUE))
  expect_true(batch_is_clean(path))
  expect_true(batch_is_clean(dirname(path)))
  expect_true(batch_is_clean(dirname(path), recurse = TRUE))
})

