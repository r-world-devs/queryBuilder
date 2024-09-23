#' Prefix used for renaming operators names
#'
#' Required due to erroneous operations on objects with names such as 'in' or 'for'.
operator_name_prefix <- "."

#' Rename operators with the provided prefix
#'
#' @param operators List storing \link{queryOperator}s.
prefix_operators_name <- function(operators) {
  names(operators) <- paste0(operator_name_prefix, names(operators))
  return(operators)
}

is_negation <- function(operator) {
  if (inherits(operator, "call") && operator[[1]] == "!") {
    return(TRUE)
  }
  FALSE
}

denegate <- function(operator) {
  if (is_negation(operator)) {
    return(operator[[-1]])
  }
  operator
}

#' Convert rule definition to expression
#'
#' @param rule Rule definition (see \link{queryRule}).
#' @param operators List storing \link{queryOperator}s.
#' @param keep_na Should the expression be extended with rule excluding/including 'NA' values?
#'
#' @return Character value storing expression to be parsed.
rule_to_expr <- function(rule, operators, keep_na = FALSE) {
  operator <- operators[[rule$operator]]
  if (is.null(operator)) {
    err_msg(
      "Operator {sQuote(rule$operator)} is not defined. Use {sQuote('setQueryOperators')} to define it."
    )
  }
  nvals <- length(rule$value)
  is_negate_opt <- is_negation(operator)
  negate_expr <- c("", "!")[c(!is_negate_opt, is_negate_opt)]
  rule_expr <- glue::glue("{negate_expr}{operator_name_prefix}{rule$operator}(`{rule$field}`, _param_)")
  if (keep_na) {
    rule_expr <- glue::glue("({rule_expr} | is.na(`{rule$field}`))")
  }
  if (nvals > 0) {
    value_expr <- utils::capture.output(dput(unlist(rule$value)))
    rule_expr <- gsub(", _param_", glue::glue(", {value_expr}, _param_"), rule_expr)
  }
  gsub(", _param_", "", rule_expr)
}

#' Convert query definition to expression
#'
#' @param query Query definition (see \link{queryRule} and \link{queryGroup}).
#' @param operators List storing \link{queryOperator}s.
#' @param conditions List storing \link{queryCondition}s.
#' @param keep_na Should each rule expression be extended with rule excluding/including 'NA' values?
#'
#' @return Character value storing expression to be parsed.
query_to_expr_bare <- function(query, operators, conditions, keep_na) {

  if (is.null(query$condition)) {
    return(rule_to_expr(query, operators, keep_na = keep_na))
  }

  if (is.null(conditions[[query$condition]])) {
    err_msg("Undefined condition used.")
  }

  expr_str <- query_to_expr_bare(query$rules[[1]], operators, conditions, keep_na = keep_na)
  if (length(query$rules) == 1) {
    return(expr_str)
  }

  for (i in 2:length(query$rules)) {
    expr_str <- glue::glue(
      "{query$condition}",
      "({expr_str}, {query_to_expr_bare(query$rules[[i]], operators, conditions, keep_na = keep_na)})"
    )
  }
  return(expr_str)
}

#' Parse query rules to R filtering expression
#'
#' The function takes a list of condition rules provided by the widget (\code{input[[<widget-name>]]}) and
#' returns valid R expression that can be used for example in \link[dplyr]{filter} function.
#'
#' @param query Query definition (see \link{queryRule} and \link{queryGroup}).
#' @param keep_na Should query keep or exclude missing values?
#' @param .queryBuilderConfig R6 class object storing query configuration. See \link{queryBuilderConfigClass}.
#'
#' @examples
#'
#' query <- queryGroup(
#'   condition = "AND",
#'   queryGroup(
#'     queryRule(
#'       field = "Species",
#'       operator = "equal",
#'       value = "setosa"
#'     ),
#'     queryRule(
#'       field = "Petal.Length",
#'       operator = "less",
#'       value = 1.2
#'     )
#'   )
#' )
#' queryToExpr(query)
#' dplyr::filter(iris, !!queryToExpr(query))
#'
#' @return Object of class 'call'. A filtering expression that can be passed to 'dplyr'-based filtering methods.
#' @export
queryToExpr <- function(query, keep_na = FALSE, .queryBuilderConfig = queryBuilderConfig) {
  operators <- .queryBuilderConfig$get_from_private(name = "operators")
  conditions <- .queryBuilderConfig$get_from_private(name = "conditions")
  is_empty_group <- length(query$rules) == 0 && is.null(query$id)

  if (is_empty_group) {
    return(TRUE)
  }
  substitute_q(
    rlang::parse_expr(query_to_expr_bare(query, operators, conditions, keep_na = keep_na)),
    c(
      purrr::map(prefix_operators_name(operators), denegate),
      conditions
    )
  )
}

stat_from_column <- function(column, column_name) {
  col_class <- class(column)[1]
  switch(
    col_class,
    "integer" = list(
      class = col_class,
      validation = list(min = min(column, na.rm = TRUE), max = max(column, na.rm = TRUE), step = 1)
    ),
    "numeric" = list(
      class = col_class,
      validation = list(min = min(column, na.rm = TRUE), max = max(column, na.rm = TRUE))
    ),
    "Date" = list(
      class = col_class,
      validation = list(min = min(column, na.rm = TRUE), max = max(column, na.rm = TRUE))
    ),
    "character" = list(
      class = col_class,
      values = stats::na.omit(unique(column))
    ),
    "factor" = list(
      class = col_class,
      values = levels(column)
    ),
    "logical" = list(
      class = col_class,
      values = stats::na.omit(unique(column))
    ),
    "POSIXct" = list(
      class = col_class,
      validation = list(min = min(column, na.rm = TRUE), max = max(column, na.rm = TRUE))
    )
  )
}
