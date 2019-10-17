context("utils")

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
