test_that("batch_process", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_true(
    batch_process(function(x) TRUE,
      path = path,
      regexp = "^file\\d[.]csv$", ask = FALSE
  ))
})

test_that("batch_process with options(seed = TRUE)", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_error(
    batch_process(function(x) TRUE,
      path = path, options = furrr::furrr_options(seed = TRUE)
    ),
    "^`options[$]seed` must be FALSE[.]$",
    class = "chk_error"
  )
})


test_that("batch_process changes files", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))

  fun <- function(file) {
    data <- read.csv(file)
    data$x <- data$x * 2
    write.csv(data, file)
    TRUE
  }

  expect_true(batch_process(fun, path, ask = FALSE))
  # should be x = 2L
  expect_identical(
    read.csv(file.path(path, "file1.csv")),
    structure(list(X.1 = 1L, X = 1L, x = 2L), class = "data.frame", row.names = c(NA, -1L))
  )
  # should be x = 6L
  expect_identical(
    read.csv(file.path(path, "file2.csv")),
    structure(list(X.1 = 1L, X = 1L, x = 6L), class = "data.frame", row.names = c(NA, -1L))
  )
})

test_that("batch_process with failure FALSE", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))

  fun <- function(file) grepl(file, "file1[.]csv$")

  expect_error(batch_config_read(path),
    "^Directory path [(]'.*'[)] must contain file '.batch.rds'[.]$",
    class = "chk_error"
  )
  expect_false(batch_process(fun, path, ask = FALSE))
  expect_error(batch_config_read(path),
    "^Directory path [(]'.*'[)] must contain file '.batch.rds'[.]$",
    class = "chk_error"
  )
})

test_that("batch_process with failure ERROR", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))

  fun <- function(file) { Sys.sleep(1e-05); stop("a problem", call. = FALSE) }

  expect_error(batch_config_read(path),
    "^Directory path [(]'.*'[)] must contain file '.batch.rds'[.]$",
    class = "chk_error"
  )
  expect_false(batch_process(fun, path, ask = FALSE))
  expect_error(batch_config_read(path),
    "^Directory path [(]'.*'[)] must contain file '.batch.rds'[.]$",
    class = "chk_error"
  )
})

test_that("batch_process with sucess character scalar", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))

  fun <- function(file) " it worked"

  expect_error(batch_config_read(path),
    "^Directory path [(]'.*'[)] must contain file '.batch.rds'[.]$",
    class = "chk_error"
  )
  expect_true(batch_process(fun, path, ask = FALSE))
  expect_error(batch_config_read(path),
    "^Directory path [(]'.*'[)] must contain file '.batch.rds'[.]$",
    class = "chk_error"
  )
})

test_that("batch_process with sucess character vector", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 3), file.path(path, "file2.csv"))

  fun <- function(file) c(" it worked", "shouldn't show")

  expect_error(batch_config_read(path),
    "^Directory path [(]'.*'[)] must contain file '.batch.rds'[.]$",
    class = "chk_error"
  )
  expect_true(batch_process(fun, path, ask = FALSE))
  expect_error(batch_config_read(path),
    "^Directory path [(]'.*'[)] must contain file '.batch.rds'[.]$",
    class = "chk_error"
  )
})
