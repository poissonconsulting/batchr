context("process")

test_that("batch_process",{
  teardown(unlink(file.path(tempdir(), "batchr_process")))
  
  path <- file.path(tempdir(), "batchr_process")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_output(batch_process(function(x) TRUE, path = path, 
                            regexp = "^file\\d[.]csv$", ask = FALSE),
                "^SUCCESS 1/1/0 \\[\\d{4,4}-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d\\] 'file1[.]csv'$")
})

test_that("batch_process changes files", {
  teardown(unlink(file.path(tempdir(), "batchr_process")))
  
  path <- file.path(tempdir(), "batchr_process")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))
  
  fun <- function(file) {
    data <- read.csv(file)
    data$x <- data$x * 2
    write.csv(data, file)
    TRUE
  }

  expect_true(batch_process(fun, path, ask = FALSE, progress = FALSE))
  # should be x = 2L
  expect_identical(read.csv(file.path(path, "file1.csv")), 
                   structure(list(X.1 = 1L, X = 1L, x = 2L), class = "data.frame", row.names = c(NA, -1L)))
  # should be x = 6L
  expect_identical(read.csv(file.path(path, "file2.csv")), 
                   structure(list(X.1 = 1L, X = 1L, x = 6L), class = "data.frame", row.names = c(NA, -1L)))
})

test_that("batch_process with failure FALSE", {
  teardown(unlink(file.path(tempdir(), "batchr_process")))
  
  path <- file.path(tempdir(), "batchr_process")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))
  
  fun <- function(file) grepl(file, "file1[.]csv$")

  expect_error(batch_config_read(path),
               "^Directory path [(]'.*batchr_process'[)] must contain file '.batch.rds'[.]$", 
               class = "chk_error")
  expect_output(batch_process(fun, path, ask = FALSE), 
                "^FAILURE 1/2/1 \\[\\d{4,4}-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d\\] 'file1[.]csv'\\s*FAILURE 2/2/2 \\[\\d{4,4}-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d\\] 'file2[.]csv'$")
  expect_false(batch_process(fun, path, ask = FALSE, progress = FALSE))
  expect_error(batch_config_read(path),
               "^Directory path [(]'.*batchr_process'[)] must contain file '.batch.rds'[.]$", 
               class = "chk_error")
})

test_that("batch_process with failure ERROR", {
  teardown(unlink(file.path(tempdir(), "batchr_process")))
  
  path <- file.path(tempdir(), "batchr_process")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))
  
  fun <- function(file) stop("a problem", call. = FALSE)

  expect_error(batch_config_read(path),
               "^Directory path [(]'.*batchr_process'[)] must contain file '.batch.rds'[.]$", 
               class = "chk_error")
  expect_output(batch_process(fun, path, ask = FALSE), 
                "^FAILURE 1/2/1 \\[\\d{4,4}-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d\\] 'file1[.]csv'\\sa problem\\sFAILURE 2/2/2 \\[\\d{4,4}-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d\\] 'file2[.]csv'\\sa problem$")
  expect_false(batch_process(fun, path, ask = FALSE, progress = FALSE))
  expect_error(batch_config_read(path),
               "^Directory path [(]'.*batchr_process'[)] must contain file '.batch.rds'[.]$", 
               class = "chk_error")
})

test_that("batch_process with sucess character scalar", {
  teardown(unlink(file.path(tempdir(), "batchr_process")))
  
  path <- file.path(tempdir(), "batchr_process")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))
  
  fun <- function(file) " it worked"

  expect_error(batch_config_read(path),
               "^Directory path [(]'.*batchr_process'[)] must contain file '.batch.rds'[.]$", 
               class = "chk_error")
  expect_output(batch_process(fun, path, ask = FALSE), 
                "^SUCCESS 1/2/0 \\[\\d{4,4}-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d\\] 'file1[.]csv'\\s\\sit worked\\s*SUCCESS 2/2/0 \\[\\d{4,4}-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d\\] 'file2[.]csv'\\s\\sit worked$")
  expect_error(batch_config_read(path),
               "^Directory path [(]'.*batchr_process'[)] must contain file '.batch.rds'[.]$", 
               class = "chk_error")
})

test_that("batch_process with sucess character vector", {
  teardown(unlink(file.path(tempdir(), "batchr_process")))
  
  path <- file.path(tempdir(), "batchr_process")
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))
  
  fun <- function(file) c(" it worked", "shouldn't show")

  expect_error(batch_config_read(path),
               "^Directory path [(]'.*batchr_process'[)] must contain file '.batch.rds'[.]$", 
               class = "chk_error")
  expect_output(batch_process(fun, path, ask = FALSE), 
                "^SUCCESS 1/2/0 \\[\\d{4,4}-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d\\] 'file1[.]csv'\\s*SUCCESS 2/2/0 \\[\\d{4,4}-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d\\] 'file2[.]csv'$")
  expect_error(batch_config_read(path),
               "^Directory path [(]'.*batchr_process'[)] must contain file '.batch.rds'[.]$", 
               class = "chk_error")
})
