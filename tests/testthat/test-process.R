context("process")

test_that("batch_process",{
  teardown(unlink(file.path(tempdir(), "batchr_start")))
  
  path <- file.path(tempdir(), "batchr_start")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_true(batch_process(function(x) TRUE, path = path, 
                                pattern = "^file\\d[.]csv$"))
})
