test_that("batch_completed", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) TRUE,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  expect_false(batch_completed(path))
  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = TRUE))
  expect_true(batch_completed(path))
})
