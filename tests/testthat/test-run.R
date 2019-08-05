context("run")

test_that("batch_run", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))
  
  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, 
                                pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  expect_identical(batch_run(path), c(file1.csv = TRUE))
  expect_identical(batch_run(path), 
                   structure(logical(0), .Names = character(0)))
})

test_that("batch_run fails all", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))
  
  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) FALSE, path = path, 
                                pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  expect_identical(batch_run(path), c(file1.csv = FALSE))
  expect_identical(batch_run(path), structure(logical(0), .Names = character(0)))
})

test_that("batch_run returns non-flag", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))
  
  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) 1, path = path, 
                                pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  expect_identical(batch_run(path), c(file1.csv = TRUE))
  expect_identical(batch_run(path), structure(logical(0), .Names = character(0)))
})

test_that("batch_run errors", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))
  
  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) stop("a problem"), path = path, 
                                pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  expect_identical(batch_run(path), c(file1.csv = FALSE))
  expect_identical(batch_run(path), structure(logical(0), .Names = character(0)))
})

test_that("batch_run locks", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))
  
  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, 
                                pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  
  # seems to need chk on CRAN  
  # res <- callr::r_safe(
  #   batch_run,
  #   list(path = path),
  #   timeout = 3,
  #   spinner = FALSE
  # )
  # expect_null(res)
})

test_that("batch_run parallel", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))
  
  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, 
                                pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  
  expect_warning(batch_run(path, parallel = TRUE), 
               "'parallel' is not used [(]yet[)]")
})

test_that("batch_run non-logger", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))
  
  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, 
                                pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  
  expect_error(batch_run(path, logger = 1), 
               "^`logger` must inherit from class 'logger'[.]$")
})

test_that("batch_run subdirectories with config", {
  teardown(unlink(file.path(tempdir(), "batchr_run", "batch_sub")))
  
  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, 
                                pattern = "^file\\d[.]csv$", recursive = TRUE),
                   "file1.csv")
  
  path <- file.path(tempdir(), "batchr_run", "batch_sub")
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, 
                                pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  
  path <- file.path(tempdir(), "batchr_run")
 
  expect_error(batch_run(path), 
               "^Subdirectories of '.*batchr_run' contain '.batchr.rds' files[.]$")
})
