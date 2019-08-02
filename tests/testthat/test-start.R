context("start")

test_that("batch_start", {
  teardown(unlink(file.path(tempdir(), "batchr_start")))
  
  path <- file.path(tempdir(), "batchr_start")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(identity, path = path, 
                                pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  expect_identical(batch_start(path), "file1.csv")
  expect_identical(batch_start(path), character(0))
})

test_that("batch_start locks", {
  teardown(unlink(file.path(tempdir(), "batchr_start")))
  
  path <- file.path(tempdir(), "batchr_start")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(identity, path = path, 
                                pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  
  # seems to need chk on CRAN  
  # res <- callr::r_safe(
  #   batch_start,
  #   list(path = path),
  #   timeout = 3,
  #   spinner = FALSE
  # )
  # expect_null(res)
})
