context("internal")

test_that("sum2intswrap", {
  mx <- 2147483647L
  expect_identical(sum2intswrap(0, 1), 1L)
  expect_identical(sum2intswrap(0, -1), -1L)
  
  expect_identical(sum2intswrap(mx, 0L), mx)
  expect_identical(sum2intswrap(-mx, 0L), -mx)

  expect_identical(sum2intswrap(mx, -1L), mx - 1L)
  expect_identical(sum2intswrap(-mx, 1L), -mx + 1L)

  expect_identical(sum2intswrap(mx, -mx), 0L)
  expect_identical(sum2intswrap(-mx, mx), 0L)
  
  expect_identical(sum2intswrap(mx, mx), 0L)
  expect_identical(sum2intswrap(-mx, -mx), 0L)
  
  expect_identical(sum2intswrap(mx, 1L), -2147483646L)
  expect_identical(sum2intswrap(mx, 2L), -2147483645L)
  expect_identical(sum2intswrap(-mx, -1L), 2147483646L)
  expect_identical(sum2intswrap(-mx, -2L), 2147483645L)
})
