pkgload::load_all()
library(dplyr)

query <- queryGroup(
  queryRule("am", "equal", 1)
)
queryToExpr(query)

query <- queryGroup(
  condition = "AND",
  queryRule("am", "equal", 1)
)
queryToExpr(query)

query <- queryGroup(
  condition = "OR",
  queryRule("am", "equal", 1),
  queryRule("vs", "equal", 1)
)
queryToExpr(query)

query <- queryGroup(
  condition = "OR",
  queryRule("am", "equal", 1),
  queryGroup(
    condition = "AND",
    queryRule("am", "equal", 0),
    queryRule("cyl", "greater", 6)
  )
)
queryToExpr(query)

query <- queryGroup(
  condition = "OR",
  queryRule("am", "equal", 1),
  queryRule("vs", "equal", 1),
  queryGroup(
    condition = "AND",
    queryRule("am", "equal", 0),
    queryRule("cyl", "greater", 6)
  )
)
queryToExpr(query)

query <- queryGroup(
  condition = "OR",
  queryGroup(
    condition = "AND",
    queryRule("am", "equal", 1),
    queryRule("cyl", "less", 6)
  ),
  queryGroup(
    condition = "AND",
    queryRule("am", "equal", 0),
    queryRule("cyl", "greater", 6)
  )
)
queryToExpr(query)

query <- queryGroup(
  condition = "AND",
  queryGroup(
    condition = "OR",
    queryRule("am", "equal", 1),
    queryRule("cyl", "less", 6)
  ),
  queryGroup(
    condition = "OR",
    queryRule("am", "equal", 0),
    queryRule("cyl", "greater", 6)
  )
)
queryToExpr(query)

setQueryConditions(
  "XOR" = queryCondition(`xor`)
)

query <- queryGroup(
  condition = "XOR",
  queryRule("am", "equal", 1),
  queryRule("vs", "equal", 1)
)
queryToExpr(query)

mtcars %>% filter(!!queryToExpr(query))

query <- queryGroup(
  queryRule("am", "in", c("am", "vs"))
)
queryToExpr(query)

query <- queryRule("am", "not_is_null")
queryToExpr(query)

query <- queryGroup(
  condition = "AND",
  queryGroup(
    condition = "OR",
    queryRule("Sepal.Length", "not_in", c(1, 2)),
    queryRule("Petal.Length", "not_between", c(6, 8))
  ),
  queryGroup(
    condition = "OR",
    queryRule("Species", "not_begins_with", "versi"),
    queryRule("Species", "not_ends_with", "sica"),
    queryRule("Species", "not_contains", "etos"),
    queryRule("Species", "not_is_empty"),
    queryRule("Species", "not_is_null")
  )
)
queryToExpr(query)
