#' Register new or list existing query conditions
#'
#' Condition is two-argument function such as `|` or `&` used to combine pair of rules.
#'
#' \itemize{
#'   \item{\code{queryCondition}: defines condition method.}
#'   \item{\code{setQueryConditions}:
#'     is used to register the defined conditions in the default or custom \link{queryBuilderConfigClass} object.
#'   }
#'   \item{\code{listQueryConditions}: returns list of registered conditions.}
#'   \item{\code{default_conditions}: an object storing default definitions for conditions.}
#' }
#'
#' @param method R function of two parameters that is used to combine a pair of rules.
#' @param ... Name-value pairs defining condition name and method respectively.
#'   Should be defined with usage of \code{queryCondition} function.
#' @param .queryBuilderConfig R6 class object storing query configuration. See \link{queryBuilderConfigClass}.
#'
#' @examples
#' setQueryConditions(
#'   "XOR" = queryCondition(xor)
#' )
#' query <- queryGroup(
#'   condition = "XOR",
#'   queryRule("am", "equal", 1),
#'   queryRule("vs", "equal", 1)
#' )
#' queryToExpr(query)
#'
#' @name query-condition
#' @export
queryCondition <- function(method) {
  condition <- substitute(method)
  return(condition)
}

#' @rdname query-condition
#' @export
setQueryConditions <- function(..., .queryBuilderConfig = queryBuilderConfig) {
  conditions <- rlang::dots_list(...)
  if (is.null(names(conditions))) {
    err_msg(
      "All the elements passed as {sQuote('...')} must be named."
    )
  }
  all_valid <- all("name" %in% lget_attr(conditions, "class"))
  if (!all_valid) {
    err_msg(
      "All the elements passed as {sQuote('...')} must be created with {sQuote('queryCondition')} function."
    )
  }
  .queryBuilderConfig$add(conditions = conditions)
  return(invisible(NULL))
}

#' @rdname query-condition
#' @param print Should the list of operators be printed into console?
#' @export
listQueryConditions <- function(.queryBuilderConfig = queryBuilderConfig, print = TRUE) {
  conditions <- .queryBuilderConfig$get_from_private(name = "conditions")
  if (print) {
    purrr::imap(conditions, ~cat(.y, ": ", deparse(.x), "\n", sep = ""))
  }
  return(invisible(conditions))
}
