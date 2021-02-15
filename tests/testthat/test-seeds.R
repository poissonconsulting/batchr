test_that("rinteger", {
  expect_error(rinteger(-1))
  expect_identical(rinteger(0), integer(0))
  set.seed(101)
  expect_identical(rinteger(1), -548903793L)
  set.seed(101)
  expect_identical(rinteger(1), -548903793L)
  expect_identical(rinteger(1), -1959257498L)
  set.seed(101)
  expect_identical(rinteger(2), c(-548903793L, -1959257498L))
})

test_that("batch_seeds", {
  expect_identical(batch_seeds(character(0)),
    structure(list(), .Names = character(0)))
  set.seed(101)
  expect_identical(batch_seeds("x"),
    list(x = c(10407L, -665974547L, 1804832414L, -1707447536L, -1371841261L,
      -1381564959L, 492098058L)))
  expect_identical(batch_seeds("y"),
    list(y = c(10407L, -1566460856L, -211444225L, 1832649274L, 765616827L,
      -1409588291L, 366035192L)))

  set.seed(101)
  expect_identical(batch_seeds("y"),
    list(y = c(10407L, -665974547L, 1804832414L, -1707447536L, -1371841261L,
      -1381564959L, 492098058L)))
  set.seed(101)
  expect_identical(batch_seeds(c("b", "aa")),
    list("b" = c(10407L, -665974547L, 1804832414L, -1707447536L, -1371841261L,
      -1381564959L, 492098058L), "aa" = c(10407L, 1220504741L, 462327187L,
      -965756182L, 1416294467L, 1761999779L, -663786240L)))

  set.seed(10)
  expect_identical(batch_seeds(c("aa", "b")),
    list("aa" = c(10407L, 1297212536L, 1045826670L, -1779609075L, 1968341300L,
      -1710900677L, -1193063059L), "b" = c(10407L, -523009245L, 2009368735L,
      379397622L, 26024764L, -450628420L, -210045713L)))
})

test_that("batch_seeds with files", {
  path <- withr::local_tempdir()

  write.csv(data.frame(x = 1), file.path(path, "file1.csv"))

  set.seed(101)

  expect_identical(
    batch_config(function(x) TRUE,
      path = path,
      regexp = "^file\\d[.]csv$"
    ),
    "file1.csv"
  )
  expect_identical(batch_seeds(batch_files_remaining(path)),
    list(file1.csv = c(10407L, -665974547L, 1804832414L, -1707447536L, -1371841261L,
      -1381564959L, 492098058L)))
})
