test_that("Construction of query is correct", {
  query <- queryGroup(
    condition = "OR",
    queryRule("am", "equal", 1),
    queryGroup(
      condition = "AND",
      queryRule("vs", "equal", 0),
      queryRule("qsec", "greater", 10),
      queryRule("wt", "less", 2.5)
    )
  )
  expect_true(!is.null(query$condition))
  expect_true(!is.null(query$rules))
  expect_equal(length(query$rules), 2)
  # second rule is a group
  expect_true(!is.null(query$rules[[2]]$condition))
  # second rule consists of three rules
  expect_equal(length(query$rules[[2]]$rules), 3)
})
