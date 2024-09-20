test_that("queryOperator returns valid quotes", {
  expect_equal(queryOperator(test), quote(test))
  test_fun <- function(x, y) {
    !x & !y
  }
  expect_equal(queryOperator(test_fun), quote(test_fun))
})
