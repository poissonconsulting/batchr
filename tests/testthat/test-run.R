test_that("batch_run", {
  path <- withr::local_tempdir()
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(
    batch_config(function(x) TRUE,
                 path = path,
                 regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = TRUE))
  expect_identical(
    batch_run(path, ask = FALSE),
    structure(logical(0), .Names = character(0))
  )
})

test_that("batch_run fails all", {
  path <- withr::local_tempdir()
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(
    batch_config(function(x) FALSE,
                 path = path,
                 regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = FALSE))
  expect_identical(batch_run(path, ask = FALSE), structure(logical(0), .Names = character(0)))
})

test_that("batch_run returns non-flag", {
  path <- withr::local_tempdir()
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(
    batch_config(function(x) 1,
                 path = path,
                 regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = TRUE))
  expect_identical(batch_run(path, ask = FALSE), structure(logical(0), .Names = character(0)))
})

test_that("batch_run errors", {
  path <- withr::local_tempdir()
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(
    batch_config(function(x) { Sys.sleep(1e-05); stop("a problem") },
                 path = path,
                 regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  expect_identical(batch_run(path, ask = FALSE), c(file1.csv = FALSE))
  expect_identical(batch_run(path, ask = FALSE), structure(logical(0), .Names = character(0)))
})

test_that("batch_run parallel with registered", {
  path <- withr::local_tempdir()
  
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
    batch_run(path, ask = FALSE),
    c(file1.csv = TRUE)
  )
})

test_that("batch_run subdirectories with config", {
  path <- withr::local_tempdir()
  sub <- withr::local_tempdir(tmpdir = path)

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  expect_identical(
    batch_config(function(x) TRUE,
                 path = path,
                 regexp = "^file\\d[.]csv$", recurse = TRUE
    ),
    "file1.csv"
  )
  
  write.csv(data.frame(x = 1), file.path(sub, "file1.csv"))
  
  expect_identical(
    batch_config(function(x) TRUE,
                 path = sub,
                 regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  
  expect_error(
    batch_run(path),
    "^Subdirectories of '.*' contain '.batchr.rds' files[.]$"
  )
})

test_that("batch_run with files specified", {
  path <- withr::local_tempdir()
  
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
  
  expect_identical(
    batch_run(path, files = c("file2.csv", "file1.csv"), ask = FALSE), c(file2.csv = TRUE, file1.csv = TRUE))
})

test_that("batch_run seed", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file2.csv"))
  
  fun <- function(x) { Sys.sleep(1e-05); stop(as.character(runif(1)), call. = TRUE) }
  
  expect_identical(
    batch_config(fun,
                 path = path,
                 regexp = "^file\\d[.]csv$"
    ),
    c("file1.csv", "file2.csv")
  )
  set.seed(101)
  expect_identical(
    batch_run(path, ask = FALSE),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )
  set.seed(101)
  expect_identical(
    batch_run(path,
              ask = FALSE,
              failed = TRUE
    ),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )
  set.seed(1)
  expect_identical(
    batch_run(path,
              ask = FALSE,
              failed = TRUE
    ),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )
  expect_identical(
    batch_run(path,
              ask = FALSE,
              failed = TRUE
    ),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )
  set.seed(101)
  expect_identical(
    batch_run(path,
              ask = FALSE,
              failed = TRUE
    ),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )
  set.seed(1)
  expect_identical(
    batch_run(path,
              ask = FALSE,
              failed = TRUE
    ),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )
  
  log <- batch_log_read(path)
  
  expect_identical(
    sort(log$message),
    sort(c("0.637362094961879", "0.889581146657672", "0.637362094961879",
      "0.889581146657672", "0.173519151073877", "0.23328331823529",
      "0.380833446097876", "0.409872261167837", "0.637362094961879",
      "0.889581146657672", "0.173519151073877", "0.23328331823529")))
  
  expect_identical(
    sort(log$file),
    sort(c(
      "file1.csv", "file2.csv", "file1.csv", "file2.csv", "file1.csv",
      "file2.csv", "file1.csv", "file2.csv", "file1.csv", "file2.csv",
      "file1.csv", "file2.csv"
    ))
  )
})

test_that("batch_run seed max", {
  path <- withr::local_tempdir()
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file2.csv"))
  
  fun <- function(x) { Sys.sleep(1e-05); stop(as.character(runif(1)), call. = TRUE) }
  
  expect_identical(
    batch_config(fun,
                 path = path,
                 regexp = "^file\\d[.]csv$"
    ),
    c("file1.csv", "file2.csv")
  )
  set.seed(2147483647L)
  expect_identical(
    batch_run(path, ask = FALSE),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )
  set.seed(2147483647L)
  expect_identical(
    batch_run(path,
              ask = FALSE,
              failed = TRUE
    ),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )
  set.seed(1)
  expect_identical(
    batch_run(path,
              ask = FALSE,
              failed = TRUE
    ),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )
  expect_identical(
    batch_run(path,
              ask = FALSE,
              failed = TRUE
    ),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )
  set.seed(2147483647L)
  expect_identical(
    batch_run(path,
              ask = FALSE,
              failed = TRUE
    ),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )
  set.seed(2147483647L)
  expect_identical(
    batch_run(path,
              ask = FALSE,
              failed = TRUE
    ),
    c(file1.csv = FALSE, file2.csv = FALSE)
  )
  
  expect_identical(
    sort(batch_log_read(path)$message),
    sort(c("0.635232788773351", "0.558590787506402", "0.635232788773351",
      "0.558590787506402", "0.173519151073877", "0.23328331823529",
      "0.380833446097876", "0.409872261167837", "0.635232788773351",
      "0.558590787506402", "0.635232788773351", "0.558590787506402"
    ))
  )
})

test_that("batch_run seed as named files errors if missing", {
  path <- withr::local_tempdir()
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  fun <- function(x) { Sys.sleep(1e-05); stop(as.character(runif(1)), call. = TRUE) }
  
  expect_identical(
    batch_config(fun,
                 path = path,
                 regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  
  expect_error(batch_run(path, seed = c(file2.csv = 1L)),
               "^`seeds` must be a list[.]$",
               class = "chk_error")
  
  expect_error(batch_run(path, seed = list(file2.csv = 1L)),
               "^`names[(]seeds[)]` must include 'file1[.]csv'[.]",
               class = "chk_error")
})

test_that("batch_run seed as named files works ignores extra ones", {
  path <- withr::local_tempdir()
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  fun <- function(x) { Sys.sleep(1e-05); stop(as.character(runif(1)), call. = TRUE) }
  
  expect_identical(
    batch_config(fun,
                 path = path,
                 regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  
  set.seed(1)
  seeds <- batch_seeds(c("file1.csv", "file2.csv"))
  expect_identical(batch_run(path, seeds = seeds, ask = FALSE),
                   c(file1.csv = FALSE))
  
  log <- batch_log_read(path)
  expect_identical(log$message, "0.173519151073877")
})

test_that("batch_run seed as named files works", {
  path <- withr::local_tempdir()
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  fun <- function(x) { Sys.sleep(1e-05); stop(as.character(runif(1)), call. = TRUE) }
  
  expect_identical(
    batch_config(fun,
                 path = path,
                 regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  seeds <- list(file1.csv = c(10407L, -348728572L, 1967489529L, 1018511380L, 1924500821L,
                              -872562238L, -388934891L))
  expect_identical(batch_run(path, seeds = seeds, ask = FALSE),
                   c(file1.csv = FALSE))
  
  log <- batch_log_read(path)
  
  expect_identical(log$message, "0.808620538607489")
  
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  fun <- function(x) { Sys.sleep(1e-05); stop(as.character(runif(1)), call. = TRUE) }
  
  expect_identical(
    batch_config(fun,
                 path = path,
                 regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  
  expect_identical(batch_run(path, seeds = seeds, ask = FALSE),
                   c(file1.csv = FALSE))
  
  log <- batch_log_read(path)
  
  expect_identical(log$message, "0.808620538607489")
  
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  fun <- function(x) { Sys.sleep(1e-05); stop(as.character(runif(1)), call. = TRUE) }
  
  expect_identical(
    batch_config(fun,
                 path = path,
                 regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  
  expect_identical(batch_run(path, seeds = list(file1.csv = c(10407L, 1767563671L, -372967108L, -1049530358L, 1484770905L,
                                                              808604029L, 190404460L)), ask = FALSE),
                   c(file1.csv = FALSE))
  
  log <- batch_log_read(path)
  
  expect_identical(log$message, "0.451294830504182")
  
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  
  fun <- function(x) { Sys.sleep(1e-05); stop(as.character(runif(1)), call. = TRUE) }
  
  expect_identical(
    batch_config(fun,
                 path = path,
                 regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  
  expect_identical(batch_run(path, seeds = seeds, ask = FALSE),
                   c(file1.csv = FALSE))
  
  log <- batch_log_read(path)
  
  expect_identical(log$message, "0.808620538607489")
  
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file2.csv"))
  
  fun <- function(x) { Sys.sleep(1e-05); stop(as.character(runif(1)), call. = TRUE) }
  
  expect_identical(
    batch_config(fun,
                 path = path,
                 regexp = "^file\\d[.]csv$"
    ),
    c("file1.csv", "file2.csv")
  )
  
  seeds <- list(file2.csv = c(10407L, 1767563671L, -372967108L, -1049530358L, 1484770905L,
                              808604029L, 190404460L),
                file1.csv = c(10407L, -348728572L, 1967489529L, 1018511380L, 1924500821L,
                              -872562238L, -388934891L))
  
  expect_identical(batch_run(path, seeds = seeds, ask = FALSE),
                   c(file1.csv = FALSE, file2.csv = FALSE))
  
  log <- batch_log_read(path)
  log <- log[order(log$file),]

  expect_identical(log$message, c("0.808620538607489", "0.451294830504182"))
  
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file2.csv"))
  
  fun <- function(x) { Sys.sleep(1e-05); stop(as.character(runif(1)), call. = TRUE) }
  
  expect_identical(
    batch_config(fun,
                 path = path,
                 regexp = "^file\\d[.]csv$"
    ),
    c("file1.csv", "file2.csv")
  )
  
  seeds <- list(file1.csv = c(10407L, 1767563671L, -372967108L, -1049530358L, 1484770905L,
                              808604029L, 190404460L),
                file2.csv = c(10407L, -348728572L, 1967489529L, 1018511380L, 1924500821L,
                              -872562238L, -388934891L))
  
  expect_identical(batch_run(path, seeds = seeds, ask = FALSE),
                   c(file1.csv = FALSE, file2.csv = FALSE))
  
  log <- batch_log_read(path)
  log <- log[order(log$file),]
  
  expect_identical(log$type,
                   c("FAILURE", "FAILURE"))

  expect_identical(log$message,
                   c("0.451294830504182", "0.808620538607489"))
  
  unlink(path, recursive = TRUE)
  dir.create(path)
  
  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
  write.csv(data.frame(x = 1), file.path(path, "file2.csv"))
  
  fun <- function(x) { Sys.sleep(1e-05); stop(as.character(runif(1)), call. = TRUE) }
  
  expect_identical(
    batch_config(fun,
                 path = path,
                 regexp = "^file\\d[.]csv$"
    ),
    c("file1.csv", "file2.csv")
  )
  
  expect_identical(batch_run(path, seeds = seeds, ask = FALSE),
                   c(file1.csv = FALSE, file2.csv = FALSE))
  
  log <- batch_log_read(path)
  log <- log[order(log$file),]
  expect_identical(log$message, c("0.451294830504182", "0.808620538607489"))
})

test_that("batch_run seed as named files parallel", {
  path <- withr::local_tempdir()
  
  options(mc.cores = 2)
  future::plan(future::multisession)
  teardown(future::plan(future::sequential))
  
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
  
  seeds <- list(file1.csv = c(10407L, 1767563671L, -372967108L, -1049530358L, 1484770905L,
                              808604029L, 190404460L),
                file2.csv = c(10407L, -348728572L, 1967489529L, 1018511380L, 1924500821L,
                              -872562238L, -388934891L))
  
  expect_identical(batch_run(path, seeds = seeds, ask = FALSE),
                   c(file1.csv = TRUE, file2.csv = TRUE))
  
  expect_identical(read.csv(file.path(path, "file1.csv"))$runif, 0.451294830504182)
  expect_identical(read.csv(file.path(path, "file2.csv"))$runif, 0.808620538607489)
})
