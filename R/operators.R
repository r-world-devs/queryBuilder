#' Register new or list existing query operators
#'
#' Operator are functions of maximum two arguments.
#' The first argument is interpreted as a field (e.g. column name),
#' the second one as a filtering value interpreted by operator accordingly.
#' Some operators, such as 'is_empty' (that compares field values to empty string) don't require any value provided.
#'
#' Operators are stored as \link{quote}s, that are further interpreted while converting the query to
#' filtering expression.
#'
#' \itemize{
#'   \item{\code{queryOperator}: defines a custom operator that can be used in generated query.}
#'   \item{\code{setQueryOperators}:
#'     is used to register the defined operators in the default or custom \link{queryBuilderConfigClass} object.
#'   }
#'   \item{\code{listQueryOperators}: allows to list available operators for the specific column type.}
#'   \item{\code{default_operators}: an object storing default definitions for operators.}
#' }
#'
#' @param ... Name-value pairs defining operator name and method respectively.
#'   Should be defined with usage of \code{queryOperator} function.
#' @param method R function the operator should be transformed to when parsing result to R expression.
#'   The function should take at most two parameters. The first one (obligatory) is variable vector,
#'   the second one additional parameters interpreted by operator.
#'   Could be negated with exclamation mark e.g. \code{queryOperator(!startsWith)} which will be interpreted as
#'   the negation of the associated expression.
#' @param .queryBuilderConfig R6 class object storing query configuration. See \link{queryBuilderConfigClass}.
#'
#' @name query-operator
#'
#' @examples
#'
#' listQueryOperators()
#'
#' in_closed_range <- function(x, range) {
#'   x >= range[1] & x <= range[2]
#' }
#'
#' setQueryOperators(
#'   "within" = queryOperator(in_closed_range),
#'   "not_within" = queryOperator(!in_closed_range)
#' )
#' query <- queryGroup(
#'   condition = "AND",
#'   queryRule("am", "equal", 1),
#'   queryRule("qsec", "within", c(10, 15)),
#'   queryRule("disp", "not_within", c(10, 15))
#' )
#' queryToExpr(query)
#'
#' @return A single `quote` storing the provided method.
#' @export
queryOperator <- function(method) {
  operator <- substitute(method)
  return(operator)
}

#' @rdname query-operator
#' @export
setQueryOperators <- function(..., .queryBuilderConfig = queryBuilderConfig) {
  operators <- rlang::dots_list(...)
  if (is.null(names(operators))) {
    err_msg(
      "All the elements passed as {sQuote('...')} must be named."
    )
  }
  all_valid <- all("name" %in% lget_attr(operators, "class"))
  if (!all_valid) {
    err_msg(
      "All the elements passed as {sQuote('...')} must be created with {sQuote('queryOperator')} function."
    )
  }
  .queryBuilderConfig$add(operators = operators)
  return(invisible(NULL))
}

#' @rdname query-operator
#' @param print Should the list of operators be printed into console?
#' @export
listQueryOperators <- function(.queryBuilderConfig = queryBuilderConfig, print = TRUE) {
  operators <- .queryBuilderConfig$get_from_private(name = "operators")
  if (print) {
    purrr::imap(operators, ~cat(.y, ": ", deparse(.x), "\n", sep = ""))
  }
  return(invisible(operators))
}

#' Check if value fits to a range
#'
#' @param x Numeric value.
#' @param range Vector of length 2, storing range change limits.
#' @return A logical vector indicating which elements of \code{x} fit into the specified \code{range}.
#' @export
in_range <- function(x, range) {
  x > range[1] & x < range[2]
}

#' Check if character value matches the provided pattern
#'
#' @param x String value.
#' @param pattern Pattern that should be matched to \code{x}.
#' @param ... Extra arguments passed to \link{grepl}.
#' @return A logical vector indicating which elements of \code{x} are matching the provided \code{pattern}.
#' @export
in_string <- function(x, pattern, ...) {
  grepl(pattern, x, ...)
}

#' Compare the string to empty value
#'
#' @param x String value.
#' @return A logical vector indicating which elements equal \code{""}.
#' @export
is_empty <- function(x) {
  x == ""
}
