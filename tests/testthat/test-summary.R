context("status")

test_that("batch_summary all processed successfully", {
  teardown(unlink(file.path(tempdir(), "batchr")))

  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) TRUE, path = path, regexp = "^file\\d[.]csv$"),
    "file1.csv"
  )

  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  verify_output(test_path("output/summary1.txt"), batch_summary(path))

  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = TRUE))

  verify_output(test_path("output/summary2.txt"), batch_summary(path))
})
