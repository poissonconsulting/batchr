context("completed")

test_that("batch_completed", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))
  
  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, 
                                regexp = "^file\\d[.]csv$"),
                   "file1.csv")
  expect_false(batch_completed(path))
  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = TRUE))
  expect_true(batch_completed(path))
})
