test_that("queryCondition returns valid quotes", {
  expect_equal(queryCondition(test), quote(test))
  test_fun <- function(x, y) {
    x < y
  }
  expect_equal(queryCondition(test_fun), quote(test_fun))
})
