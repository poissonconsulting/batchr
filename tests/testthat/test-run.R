context("run")

test_that("batch_run", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))

  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) TRUE,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE), c(file1.csv = TRUE))
  expect_identical(
    batch_run(path, ask = FALSE, progress = FALSE),
    structure(logical(0), .Names = character(0))
  )
})

test_that("batch_run fails all", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))

  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) FALSE,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE), c(file1.csv = FALSE))
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE), structure(logical(0), .Names = character(0)))
})

test_that("batch_run returns non-flag", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))

  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) 1,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE), c(file1.csv = TRUE))
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE), structure(logical(0), .Names = character(0)))
})

test_that("batch_run errors", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))

  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) stop("a problem"),
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE), c(file1.csv = FALSE))
  expect_identical(batch_run(path, ask = FALSE, progress = FALSE), structure(logical(0), .Names = character(0)))
})

test_that("batch_run parallel with registered", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))

  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) TRUE,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )

  options(mc.cores = 2)
  future::plan(future::multisession)
  teardown(future::plan(future::sequential))

  expect_identical(
    batch_run(path, parallel = TRUE, ask = FALSE),
    c(file1.csv = TRUE)
  )
})

test_that("batch_run subdirectories with config", {
  teardown(unlink(file.path(tempdir(), "batchr_run", "batch_sub")))

  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) TRUE,
      path = path,
      regexp = "^file\\d[.]csv$", recurse = TRUE
    ),
    "file1.csv"
  )

  path <- file.path(tempdir(), "batchr_run", "batch_sub")
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  expect_identical(
    batch_config(function(x) TRUE,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )

  path <- file.path(tempdir(), "batchr_run")

  expect_error(
    batch_run(path, progress = FALSE),
    "^Subdirectories of '.*batchr_run' contain '.batchr.rds' files[.]$"
  )
})

test_that("batch_run with files specified", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))

  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 2), file.path(path, "file2.csv"))

  expect_identical(
    batch_config(function(x) TRUE,
      path = path,
      regexp = "^file\\d[.]csv$", recurse = TRUE
    ),
    c("file1.csv", "file2.csv")
  )

  expect_error(
    batch_run(path, files = "file.csv"),
    "^The following files are not remaining: 'file.csv'.$"
  )

  expect_identical(
    batch_run(path, files = character(0)),
    structure(logical(0), .Names = character(0))
  )

  expect_error(
    batch_run(path, files = c("file3.csv", "file.csv")),
    "^The following files are not remaining: 'file3.csv' and 'file.csv'.$"
  )

  expect_error(
    batch_run(path, files = c("file3.csv", "file1.csv")),
    "^The following files are not remaining: 'file3.csv'.$"
  )

  expect_output(
    batch_run(path, files = c("file2.csv", "file1.csv"), ask = FALSE),
    "^SUCCESS 1/2/0 \\[\\d{4,4}-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d\\] 'file2[.]csv'\\s*SUCCESS 2/2/0 \\[\\d{4,4}-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d\\] 'file1[.]csv'$"
  )
})

test_that("batch_run seed", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))

  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file2.csv"))

  fun <- function(x) stop(as.character(runif(1)), call. = TRUE)

  expect_identical(
    batch_config(fun,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    c("file1.csv", "file2.csv")
  )
  set.seed(101)
  expect_identical(
    batch_run(path, ask = FALSE, progress = FALSE),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )
  set.seed(101)
  expect_identical(
    batch_run(path,
      ask = FALSE, progress = FALSE,
      failed = TRUE
    ),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )
  set.seed(1)
  expect_identical(
    batch_run(path,
      ask = FALSE, progress = FALSE,
      failed = TRUE
    ),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )
  expect_identical(
    batch_run(path,
      ask = FALSE, progress = FALSE,
      failed = TRUE
    ),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )
  set.seed(101)
  expect_identical(
    batch_run(path,
      ask = FALSE, progress = FALSE,
      failed = TRUE
    ),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )
  set.seed(1)
  expect_identical(
    batch_run(path,
      ask = FALSE, progress = FALSE,
      failed = TRUE
    ),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )


  expect_identical(
    batch_log_read(path)$message,
    c("0.780211817938834", "0.824853724101558", "0.780211817938834", 
"0.824853724101558", "0.386242696549743", "0.47574620670639", 
"0.828081487910822", "0.0105598932132125", "0.780211817938834", 
"0.824853724101558", "0.386242696549743", "0.47574620670639"))

  expect_identical(
    batch_log_read(path)$file,
    c(
      "file1.csv", "file2.csv", "file1.csv", "file2.csv", "file1.csv",
      "file2.csv", "file1.csv", "file2.csv", "file1.csv", "file2.csv",
      "file1.csv", "file2.csv"
    )
  )
})

test_that("batch_run seed max", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))

  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file2.csv"))

  fun <- function(x) stop(as.character(runif(1)), call. = TRUE)

  expect_identical(
    batch_config(fun,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    c("file1.csv", "file2.csv")
  )
  set.seed(2147483647L)
  expect_identical(
    batch_run(path, ask = FALSE, progress = FALSE),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )
  set.seed(2147483647L)
  expect_identical(
    batch_run(path,
      ask = FALSE, progress = FALSE,
      failed = TRUE
    ),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )
  set.seed(1)
  expect_identical(
    batch_run(path,
      ask = FALSE, progress = FALSE,
      failed = TRUE
    ),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )
  expect_identical(
    batch_run(path,
      ask = FALSE, progress = FALSE,
      failed = TRUE
    ),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )
  set.seed(2147483647L)
  expect_identical(
    batch_run(path,
      ask = FALSE, progress = FALSE,
      failed = TRUE
    ),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )
  set.seed(2147483647L)
  expect_identical(
    batch_run(path,
      ask = FALSE, progress = FALSE,
      failed = TRUE
    ),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )


  expect_identical(
    batch_log_read(path)$message,
    c("0.188387020025402", "0.733740209368989", "0.188387020025402", 
"0.733740209368989", "0.386242696549743", "0.47574620670639", 
"0.828081487910822", "0.0105598932132125", "0.188387020025402", 
"0.733740209368989", "0.188387020025402", "0.733740209368989"
)
  )
})

test_that("batch_run seed as named files errors if missing", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))

  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  fun <- function(x) stop(as.character(runif(1)), call. = TRUE)

  expect_identical(
    batch_config(fun,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  
  expect_error(batch_run(path, seed = c(file2.csv = 1L)),
               "^`names[(]seed[)]` must include 'file1[.]csv'[.]",
               class = "chk_error")
})


test_that("batch_run seed as named files works ignores extra ones", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))

  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  fun <- function(x) stop(as.character(runif(1)), call. = TRUE)

  expect_identical(
    batch_config(fun,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )

  expect_identical(batch_run(path, seed = c(file1.csv = 1L, file2.csv = 3L), ask = FALSE,
                         progress = FALSE),
               c(file1.csv = FALSE))
  
  expect_identical(batch_log_read(path)$message,
                   "0.033562890952453")
})

test_that("batch_run seed as named files works", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))

  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  fun <- function(x) stop(as.character(runif(1)), call. = TRUE)

  expect_identical(
    batch_config(fun,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )

  expect_identical(batch_run(path, seed = c(file1.csv = 1L), ask = FALSE,
                         progress = FALSE),
               c(file1.csv = FALSE))
  
  expect_identical(batch_log_read(path)$message,
                   "0.033562890952453")
  
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  fun <- function(x) stop(as.character(runif(1)), call. = TRUE)

  expect_identical(
    batch_config(fun,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )

  expect_identical(batch_run(path, seed = c(file1.csv = 1L), ask = FALSE,
                         progress = FALSE),
               c(file1.csv = FALSE))
  
  expect_identical(batch_log_read(path)$message,
                   "0.033562890952453")
  
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  fun <- function(x) stop(as.character(runif(1)), call. = TRUE)

  expect_identical(
    batch_config(fun,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )

  expect_identical(batch_run(path, seed = c(file1.csv = 2L), ask = FALSE,
                         progress = FALSE),
               c(file1.csv = FALSE))
  
  expect_identical(batch_log_read(path)$message,
                   "0.11178148444742")
  
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  fun <- function(x) stop(as.character(runif(1)), call. = TRUE)

  expect_identical(
    batch_config(fun,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )

  expect_identical(batch_run(path, seed = 1L, ask = FALSE,
                         progress = FALSE),
               c(file1.csv = FALSE))
  
  expect_identical(batch_log_read(path)$message,
                   "0.033562890952453")
  
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file2.csv"))

  fun <- function(x) stop(as.character(runif(1)), call. = TRUE)

  expect_identical(
    batch_config(fun,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    c("file1.csv", "file2.csv")
  )

  expect_identical(batch_run(path, seed = 1L, ask = FALSE,
                         progress = FALSE),
               c(file1.csv = FALSE, file2.csv = FALSE))
  
  expect_identical(batch_log_read(path)$message,
                   c("0.033562890952453", "0.418956762179732"))
  
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file2.csv"))

  fun <- function(x) stop(as.character(runif(1)), call. = TRUE)

  expect_identical(
    batch_config(fun,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    c("file1.csv", "file2.csv")
  )

  expect_identical(batch_run(path, seed = 2L, ask = FALSE,
                         progress = FALSE),
               c(file1.csv = FALSE, file2.csv = FALSE))
  
  expect_identical(batch_log_read(path)$message,
                   c("0.11178148444742", "0.364263514755294"))

  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file2.csv"))

  fun <- function(x) stop(as.character(runif(1)), call. = TRUE)

  expect_identical(
    batch_config(fun,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    c("file1.csv", "file2.csv")
  )

  expect_identical(batch_run(path, seed = c(file1.csv = 2L, file2.csv = 1L), ask = FALSE,
                         progress = FALSE),
               c(file1.csv = FALSE, file2.csv = FALSE))
  
  expect_identical(batch_log_read(path)$message,
                   c("0.11178148444742", "0.418956762179732"))
})

test_that("batch_run seed as named files parallel", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))

  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file2.csv"))

  fun <- function(x) {
    dat <- read.csv(x)
    dat$runif <- runif(1)
    write.csv(dat, x)
  }
  
  expect_identical(
    batch_config(fun,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    c("file1.csv", "file2.csv")
  )

  expect_identical(batch_run(path, seed = c(file1.csv = 2L, file2.csv = 1L), ask = FALSE,
                         progress = FALSE),
               c(file1.csv = TRUE, file2.csv = TRUE))
  
  expect_identical(read.csv(file.path(path, "file1.csv"))$runif, 0.11178148444742)
  expect_identical(read.csv(file.path(path, "file2.csv"))$runif, 0.418956762179732)
})

test_that("batch_run seed as named files parallel", {
  teardown(unlink(file.path(tempdir(), "batchr_run")))

  path <- file.path(tempdir(), "batchr_run")
  unlink(path, recursive = TRUE)
  dir.create(path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file2.csv"))

  fun <- function(x) {
    dat <- read.csv(x)
    dat$runif <- runif(1)
    write.csv(dat, x)
  }
  
  expect_identical(
    batch_config(fun,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    c("file1.csv", "file2.csv")
  )

  expect_identical(batch_run(path, seed = c(file1.csv = 1L, file2.csv = 2L), ask = FALSE,
                         progress = FALSE),
               c(file1.csv = TRUE, file2.csv = TRUE))
  
  expect_identical(read.csv(file.path(path, "file1.csv"))$runif, 0.033562890952453)
  expect_identical(read.csv(file.path(path, "file2.csv"))$runif, 0.364263514755294)
})
