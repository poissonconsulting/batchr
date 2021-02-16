test_that("demo", {
  path <- withr::local_tempdir()

  writeLines("the contents of file.txt", file.path(path, "file.txt"))
  writeLines("the contents of file2.txt", file.path(path, "file2.txt"))
  writeLines("the contents of file3.txt", file.path(path, "file3.txt"))
  writeLines("the contents of file4.txt", file.path(path, "file4.txt"))

  fun <- function(file) {
    if (grepl("file3[.]txt$", file)) {
      return(FALSE)
    }
    if (grepl("file4[.]txt$", file)) stop("Uh, Houston, we've had a problem.", call. = FALSE)
    txt <- readLines(file)
    txt <- gsub("contents", "modified contents", txt)
    writeLines(txt, file)
  }

  expect_identical(
    batch_config(fun, path = path, regexp = "file\\d[.]txt$"),
    c("file2.txt", "file3.txt", "file4.txt")
  )

  config <- batch_config_read(path)

  expect_identical(
    names(config),
    c("time", "regexp", "recurse", "fun", "dots")
  )

  expect_identical(
    batch_run(path, ask = FALSE),
    c(file2.txt = TRUE, file3.txt = FALSE, file4.txt = FALSE)
  )
  
  log <- batch_log_read(path)
  log <- log[order(log$file),]
  
  expect_identical(log$type, c("SUCCESS", "FAILURE", "FAILURE"))
  expect_identical(log$message, c(NA, NA, "Uh, Houston, we've had a problem."))
  
  expect_identical(
    readLines(file.path(path, "file.txt")),
    "the contents of file.txt"
  )
  expect_identical(
    readLines(file.path(path, "file2.txt")),
    "the modified contents of file2.txt"
  )
  expect_identical(
    readLines(file.path(path, "file3.txt")),
    "the contents of file3.txt"
  )
  expect_identical(
    readLines(file.path(path, "file4.txt")),
    "the contents of file4.txt"
  )

  fun <- function(file) {
    txt <- readLines(file)
    txt <- gsub("contents", "modified contents", txt)
    writeLines(txt, file)
  }

  batch_reconfig_fun(path, fun)
  batch_reconfig_fileset(path, regexp = "[.]txt$")

  reconfig <- batch_config_read(path)
  expect_identical(names(reconfig), names(config))
  expect_identical(reconfig$time, config$time)
  expect_identical(reconfig$recurse, config$recurse)
  expect_identical(reconfig$dots, config$dots)

  expect_identical(batch_run(path, ask = FALSE), c(file.txt = TRUE))
  expect_identical(batch_run(path, ask = FALSE), c(x = TRUE)[-1])

  expect_warning(expect_identical(batch_cleanup(path), c(. = FALSE)),
                 "^Clean up of 1 file failed[.]$")
  expect_identical(batch_run(path, ask = FALSE, failed = NA), c(file3.txt = TRUE, file4.txt = TRUE))
  expect_identical(batch_cleanup(path), c("." = TRUE))
})
