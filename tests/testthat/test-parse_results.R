test_that("rule_to_expr converts single rules to expressions with bare (but prefixed) operators ids", {
  rule <- queryRule("am", "equal", 1)
  expect_equal(
    rule_to_expr(rule, operators = default_operators, keep_na = FALSE),
    ".equal(`am`, 1)"
  )
  expect_equal(
    rule_to_expr(rule, operators = default_operators, keep_na = TRUE),
    "(.equal(`am`, 1) | is.na(`am`))"
  )

  rule <- queryRule("am", "is_null")
  expect_equal(
    rule_to_expr(rule, operators = default_operators, keep_na = FALSE),
    ".is_null(`am`)"
  )
})

test_that("query_to_expr_bare converts queries with bare ids of operators (prefixed with .) and conditions", {
  query <- queryRule("am", "equal", 1)
  expect_equal(
    query_to_expr_bare(query, default_operators, default_conditions, keep_na = FALSE),
    ".equal(`am`, 1)"
  )
  expect_equal(
    query_to_expr_bare(query, default_operators, default_conditions, keep_na = TRUE),
    "(.equal(`am`, 1) | is.na(`am`))"
  )

  query <- queryGroup(
    condition = "OR",
    queryRule("am", "equal", 1),
    queryRule("vs", "equal", 0)
  )
  expect_equal(
    query_to_expr_bare(query, default_operators, default_conditions, keep_na = FALSE),
    "OR(.equal(`am`, 1), .equal(`vs`, 0))"
  )
  expect_equal(
    query_to_expr_bare(query, default_operators, default_conditions, keep_na = TRUE),
    "OR((.equal(`am`, 1) | is.na(`am`)), (.equal(`vs`, 0) | is.na(`vs`)))"
  )

  query <- queryGroup(
    condition = "OR",
    queryRule("am", "equal", 1),
    queryGroup(
      condition = "AND",
      queryRule("vs", "equal", 0),
      queryRule("qsec", "greater", 10)
    )
  )
  expect_equal(
    query_to_expr_bare(query, default_operators, default_conditions, keep_na = FALSE),
    "OR(.equal(`am`, 1), AND(.equal(`vs`, 0), .greater(`qsec`, 10)))"
  )

  expect_equal(
    query_to_expr_bare(query, default_operators, default_conditions, keep_na = TRUE),
    "OR((.equal(`am`, 1) | is.na(`am`)), AND((.equal(`vs`, 0) | is.na(`vs`)), (.greater(`qsec`, 10) | is.na(`qsec`))))"
  )
})

test_that("queryToExpr converts queries to expression as intended", {
  queryBuilderConfig$reset()

  query <- queryRule("am", "equal", 1)
  queryToExpr(query)

  query <- queryRule(
    "am",
    "not_existing"
  )
  expect_error(queryToExpr(query), "Operator .*not_existing.* is not defined.")

  query <- queryGroup(
    condition = "OR",
    queryRule("am", "equal", 1),
    queryGroup(
      condition = "AND",
      queryRule("vs", "equal", 0),
      queryRule("qsec", "greater", 10)
    )
  )
  expect_equal(queryToExpr(query), quote(am == 1 | vs == 0 & qsec > 10))

  query <- queryGroup(
    condition = "XOR",
    queryRule("am", "equal", 1),
    queryGroup(
      condition = "AND",
      queryRule("vs", "non_zero"),
      queryRule("qsec", "greater", 10)
    )
  )
  expect_error(queryToExpr(query), "Undefined condition used.")

  setQueryConditions(XOR = queryCondition(xor))
  non_zero <- function(x) {
    abs(x) > 0
  }
  setQueryOperators(non_zero = queryOperator(non_zero))
  expect_equal(queryToExpr(query), quote(xor(am == 1, non_zero(vs) & qsec > 10)))
  mtc_filtered <- dplyr::filter(mtcars, xor(am == 1, non_zero(vs) & qsec > 10))
  expect_equal(mtc_filtered, dplyr::filter(mtcars, !!queryToExpr(query)))

  expect_equal(
    queryToExpr(query, keep_na = TRUE),
    quote(xor((am == 1 | is.na(am)), (non_zero(vs) | is.na(vs)) & (qsec > 10 | is.na(qsec))))
  )

  queryBuilderConfig$reset()
})
