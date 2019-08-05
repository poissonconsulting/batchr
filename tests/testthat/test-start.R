context("start")

test_that("batch_start", {
  teardown(unlink(file.path(tempdir(), "batchr_start")))
  
  path <- file.path(tempdir(), "batchr_start")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, 
                                pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  expect_identical(batch_start(path), c(file1.csv = TRUE))
  expect_identical(batch_start(path), 
                   structure(logical(0), .Names = character(0)))
})

test_that("batch_start fails all", {
  teardown(unlink(file.path(tempdir(), "batchr_start")))
  
  path <- file.path(tempdir(), "batchr_start")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) FALSE, path = path, 
                                pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  expect_identical(batch_start(path), c(file1.csv = FALSE))
  expect_identical(batch_start(path), structure(logical(0), .Names = character(0)))
})

test_that("batch_start returns non-flag", {
  teardown(unlink(file.path(tempdir(), "batchr_start")))
  
  path <- file.path(tempdir(), "batchr_start")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) 1, path = path, 
                                pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  expect_identical(batch_start(path), c(file1.csv = TRUE))
  expect_identical(batch_start(path), structure(logical(0), .Names = character(0)))
})

test_that("batch_start errors", {
  teardown(unlink(file.path(tempdir(), "batchr_start")))
  
  path <- file.path(tempdir(), "batchr_start")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) stop("a problem"), path = path, 
                                pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  expect_identical(batch_start(path), c(file1.csv = FALSE))
  expect_identical(batch_start(path), structure(logical(0), .Names = character(0)))
})

test_that("batch_start locks", {
  teardown(unlink(file.path(tempdir(), "batchr_start")))
  
  path <- file.path(tempdir(), "batchr_start")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, 
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

test_that("batch_start parallel", {
  teardown(unlink(file.path(tempdir(), "batchr_start")))
  
  path <- file.path(tempdir(), "batchr_start")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, 
                                pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  
  expect_warning(batch_start(path, parallel = TRUE), 
               "'parallel' is not used [(]yet[)]")
})

test_that("batch_start non-logger", {
  teardown(unlink(file.path(tempdir(), "batchr_start")))
  
  path <- file.path(tempdir(), "batchr_start")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, 
                                pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  
  expect_error(batch_start(path, logger = 1), 
               "^`logger` must inherit from class 'logger'[.]$")
})

test_that("batch_start subdirectories with config", {
  teardown(unlink(file.path(tempdir(), "batchr_start", "batch_sub")))
  
  path <- file.path(tempdir(), "batchr_start")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, 
                                pattern = "^file\\d[.]csv$", recursive = TRUE),
                   "file1.csv")
  
  path <- file.path(tempdir(), "batchr_start", "batch_sub")
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, 
                                pattern = "^file\\d[.]csv$"),
                   "file1.csv")
  
  path <- file.path(tempdir(), "batchr_start")
 
  expect_error(batch_start(path), 
               "^Subdirectories of '.*batchr_start' contain '.batchr.rds' files[.]$")
})
