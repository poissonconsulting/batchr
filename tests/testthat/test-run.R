context("run")

test_that("batch_run", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))
  
  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, 
                                regexp = "^file\\d[.]csv$"),
                   "file1.csv")
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE), c(file1.csv = TRUE))
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE), 
                   structure(logical(0), .Names = character(0)))
})

test_that("batch_run fails all", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))
  
  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) FALSE, path = path, 
                                regexp = "^file\\d[.]csv$"),
                   "file1.csv")
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE), c(file1.csv = FALSE))
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE), structure(logical(0), .Names = character(0)))
})

test_that("batch_run returns non-flag", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))
  
  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) 1, path = path, 
                                regexp = "^file\\d[.]csv$"),
                   "file1.csv")
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE), c(file1.csv = TRUE))
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE), structure(logical(0), .Names = character(0)))
})

test_that("batch_run errors", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))
  
  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) stop("a problem"), path = path, 
                                regexp = "^file\\d[.]csv$"),
                   "file1.csv")
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE), c(file1.csv = FALSE))
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE), structure(logical(0), .Names = character(0)))
})

test_that("batch_run parallel without registered", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))
  
  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, 
                                regexp = "^file\\d[.]csv$"),
                   "file1.csv")
  
  expect_warning(batch_run(path, parallel = TRUE, ask = FALSE),
                 "(n|N)o parallel backend registered")
})

test_that("batch_run parallel with registered", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))
  
  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, 
                                regexp = "^file\\d[.]csv$"),
                   "file1.csv")
  
  doParallel::registerDoParallel(2)
  teardown(doParallel::stopImplicitCluster())
  
  expect_identical(batch_run(path, parallel = TRUE, ask = FALSE), 
                   c(file1.csv = TRUE))
})

test_that("batch_run subdirectories with config", {
  teardown(unlink(file.path(tempdir(), "batchr_run", "batch_sub")))
  
  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, 
                                regexp = "^file\\d[.]csv$", recurse = TRUE),
                   "file1.csv")
  
  path <- file.path(tempdir(), "batchr_run", "batch_sub")
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, 
                                regexp = "^file\\d[.]csv$"),
                   "file1.csv")
  
  path <- file.path(tempdir(), "batchr_run")
  
  expect_error(batch_run(path, progress = FALSE), 
               "^Subdirectories of '.*batchr_run' contain '.batchr.rds' files[.]$")
})

test_that("batch_run with files specified", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))
  
  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 2), file.path(path, "file2.csv"))
  
  expect_identical(batch_config(function(x) TRUE, path = path, 
                                regexp = "^file\\d[.]csv$", recurse = TRUE),
                   c("file1.csv", "file2.csv"))
  
  expect_error(batch_run(path, files = "file.csv"), 
               "^The following files are not remaining: 'file.csv'.$")
  
  expect_identical(batch_run(path, files = character(0)), 
                   structure(logical(0), .Names = character(0)))
  
  expect_error(batch_run(path, files = c("file3.csv", "file.csv")), 
               "^The following files are not remaining: 'file3.csv' and 'file.csv'.$")
  
  expect_error(batch_run(path, files = c("file3.csv", "file1.csv")), 
               "^The following files are not remaining: 'file3.csv'.$")
  
  expect_output(batch_run(path, files = c("file2.csv", "file1.csv"), ask = FALSE),
                "^SUCCESS 1/2/0 \\[\\d{4,4}-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d\\] 'file2[.]csv'\\s*SUCCESS 2/2/0 \\[\\d{4,4}-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d\\] 'file1[.]csv'$")
})

test_that("batch_run seed", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))
  
  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file2.csv"))
  
  fun <- function(x) stop(as.character(runif(1)), call. = TRUE)
  
  expect_identical(batch_config(fun, path = path, 
                                regexp = "^file\\d[.]csv$"),
                   c("file1.csv", "file2.csv"))
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE,
                             seed = 101), 
                   c(file1.csv = FALSE, file2.csv = FALSE))
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE,
                             seed = 101, failed = TRUE), 
                   c(file1.csv = FALSE, file2.csv = FALSE))
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE,
                             seed = 1, failed = TRUE), 
                   c(file1.csv = FALSE, file2.csv = FALSE))
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE,
                             failed = TRUE), 
                   c(file1.csv = FALSE, file2.csv = FALSE))
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE,
                             seed = 101, failed = TRUE), 
                   c(file1.csv = FALSE, file2.csv = FALSE))
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE,
                             seed = 1, failed = TRUE), 
                   c(file1.csv = FALSE, file2.csv = FALSE))
  
  
  expect_identical(batch_log_read(path)$message, 
                   c("0.313109797192737", "0.484942433657125", "0.313109797192737", 
"0.484942433657125", "0.691987211350352", "0.386118894442916", 
"0.89029288222082", "0.0763199771754444", "0.313109797192737", 
"0.484942433657125", "0.691987211350352", "0.386118894442916"
))
  
  expect_identical(batch_log_read(path)$file, 
                   c("file1.csv", "file2.csv", "file1.csv", "file2.csv", "file1.csv", 
                     "file2.csv", "file1.csv", "file2.csv", "file1.csv", "file2.csv", 
                     "file1.csv", "file2.csv"))
})

test_that("batch_run seed max", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))
  
  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file2.csv"))
  
  fun <- function(x) stop(as.character(runif(1)), call. = TRUE)
  
  expect_identical(batch_config(fun, path = path, 
                                regexp = "^file\\d[.]csv$"),
                   c("file1.csv", "file2.csv"))
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE,
                             seed = 2147483647L), 
                   c(file1.csv = FALSE, file2.csv = FALSE))
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE,
                             seed = 2147483647L, failed = TRUE), 
                   c(file1.csv = FALSE, file2.csv = FALSE))
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE,
                             seed = 1, failed = TRUE), 
                   c(file1.csv = FALSE, file2.csv = FALSE))
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE,
                             failed = TRUE), 
                   c(file1.csv = FALSE, file2.csv = FALSE))
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE,
                             seed = 2147483647L, failed = TRUE), 
                   c(file1.csv = FALSE, file2.csv = FALSE))
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE,
                             seed = 2147483647L, failed = TRUE), 
                   c(file1.csv = FALSE, file2.csv = FALSE))
  
  
  expect_identical(batch_log_read(path)$message, 
                   c("0.489229316823184", "0.329910852015018", "0.489229316823184", 
"0.329910852015018", "0.691987211350352", "0.386118894442916", 
"0.89029288222082", "0.0763199771754444", "0.489229316823184", 
"0.329910852015018", "0.489229316823184", "0.329910852015018"))
})
