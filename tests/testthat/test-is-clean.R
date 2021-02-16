test_that("batch_is_clean()", {
  path <- withr::local_tempdir()
  
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

test_that("batch_is_clean() recurse", {
  path <- withr::local_tempdir()
  sub <- withr::local_tempdir(tmpdir = path)

  write.csv(data.frame(x = 1), file.path(sub, "file1.csv"))

  expect_true(batch_is_clean(sub))
  expect_true(batch_is_clean(dirname(sub)))
  expect_true(batch_is_clean(dirname(sub), recurse = TRUE))

  expect_identical(
    batch_config(function(x) TRUE,
      path = sub,
      regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  expect_false(batch_is_clean(sub))
  expect_true(batch_is_clean(dirname(sub)))
  expect_false(batch_is_clean(dirname(sub), recurse = TRUE))

  expect_identical(batch_cleanup(sub, force = TRUE), c(. = TRUE))
  expect_true(batch_is_clean(sub))
  expect_true(batch_is_clean(dirname(sub)))
  expect_true(batch_is_clean(dirname(sub), recurse = TRUE))
})
