context("files")

test_that("batch_setup_files", {
  teardown(unlink(file.path(tempdir(), "batchr")))
  
  path <- file.path(tempdir(), "batchr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  batch_setup_files()
  expect_identical(batch_setup(identity, path = path, pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  batch_setup_files()
})
