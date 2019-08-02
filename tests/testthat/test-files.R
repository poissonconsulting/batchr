context("files")

test_that("batch_config_files", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config_files(path), character(0))
  expect_identical(batch_config(identity, path = path, pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  expect_identical(batch_config_files(path), ".batchr.rds")
})

