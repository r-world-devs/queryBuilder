#' Define filtering query
#'
#' Query is configuration consisting of rules and group.
#' Rule defines a single filtering expression whereas group is combining multiple rules (or nested groups)
#' with the provided condition.
#'
#' Having the example expression `a == 1 | (vs == 0 & qsec > 10)` we can distinct the following rules and groups:
#'
#' Rules:
#' - `am == 1` - related to `am` field, applies `==` operator with `1` value,
#' - `vs == 0` - related to `vs` field, applies `==` operator with `1` value,
#' - `qsec > 10` - related to `qsec` field, applies `>` operator with `10` value.
#'
#' Groups:
#' - `(vs == 0 & qsec > 10)` - combines two rules (`vs == 0` and `qsec > 10`) with `&` condition,
#' - `a == 1 | (vs == 0 & qsec > 10)` - combines rule `a == 1` and group `(vs == 0 & qsec > 10)` with `|` condition.
#'
#' Such query can be defined by 'queryBuilder' the following way:
#'
#' \code{
#'   queryGroup(
#'     condition = "OR",
#'     queryRule("am", "equal", 1)
#'     queryGroup(
#'       condition = "AND",
#'       queryRule("vs", "equal", 0),
#'       queryRule("qsec", "greater", 10)
#'     )
#'   )
#' }
#'
#' Connection between conditions and operators names and their R-based counterparts are defined with
#' \link{queryBuilderConfig} class.
#'
#' The defined query can be then converted to filtering expression with \link{queryToExpr} function.
#'
#' @param ... Rules defined with \code{queryRule} function.
#' @param condition Group condition. By default 'AND' and 'OR' are available.
#'   To set custom one use \link{setQueryConditions}.
#' @param field Field of the filter applied to the rule.
#'   To set custom one use \link{setQueryOperators}.
#' @param operator Name of the operator to be applied to the rule.
#' @param value (optional) Values that should be applied to the rule.
#'    Some operators, such as 'is_null', don't require any value provided.
#'
#' @examples
#'
#' queryGroup(
#'   condition = "OR",
#'   queryRule("am", "equal", 1),
#'   queryGroup(
#'     condition = "AND",
#'     queryRule("vs", "equal", 0),
#'     queryRule("qsec", "greater", 10)
#'   )
#' )
#'
#' @name query-rules
#' @return Nested lists structure.
#'
#' @export
queryGroup <- function(..., condition = "AND") {
  rules <- list(
    condition = condition,
    rules = list(...)
  )
  return(rules)
}

#' @export
#' @rdname query-rules
queryRule <- function(field, operator, value = NULL, ...) {
  list(
    id = field,
    field = field,
    operator = operator,
    value = value,
    ...
  )
}
