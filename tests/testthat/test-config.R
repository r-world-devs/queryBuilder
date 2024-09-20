test_that("Operating on built-in query configuration works", {
  queryBuilderConfig$reset()

  setQueryConditions(XOR = queryCondition(xor))
  expect_true("XOR" %in% names(listQueryConditions(print = FALSE)))

  queryBuilderConfig$remove(conditions_id = "XOR")
  expect_false("XOR" %in% names(listQueryConditions(print = FALSE)))

  non_zero <- function(x) {
    abs(x) > 0
  }
  setQueryOperators(non_zero = queryCondition(non_zero))
  expect_true("non_zero" %in% names(listQueryOperators(print = FALSE)))

  queryBuilderConfig$remove(operators_id = "non_zero")
  expect_false("non_zero" %in% names(listQueryOperators(print = FALSE)))

  queryBuilderConfig$reset()
  expect_identical(listQueryOperators(print = FALSE), default_operators)
  expect_identical(listQueryConditions(print = FALSE), default_conditions)

  queryBuilderConfig$reset()
})

test_that("Operating on new query configuration works", {
  qbc <- queryBuilderConfigClass$new()

  setQueryConditions(XOR = queryCondition(xor), .queryBuilderConfig = qbc)
  expect_true("XOR" %in% names(listQueryConditions(.queryBuilderConfig = qbc, print = FALSE)))

  qbc$remove(conditions_id = "XOR")
  expect_false("XOR" %in% names(listQueryConditions(.queryBuilderConfig = qbc, print = FALSE)))

  non_zero <- function(x) {
    abs(x) > 0
  }
  setQueryOperators(non_zero = queryCondition(non_zero), .queryBuilderConfig = qbc)
  expect_true("non_zero" %in% names(listQueryOperators(.queryBuilderConfig = qbc, print = FALSE)))

  qbc$remove(operators_id = "non_zero")
  expect_false("non_zero" %in% names(listQueryOperators(.queryBuilderConfig = qbc, print = FALSE)))

  qbc$reset()
  expect_identical(listQueryOperators(.queryBuilderConfig = qbc, print = FALSE), default_operators)
  expect_identical(listQueryConditions(.queryBuilderConfig = qbc, print = FALSE), default_conditions)

  qbc$reset()
})
