#' @rdname query-operator
#' @export
default_operators <- list(
  "equal" = queryOperator(`==`),
  "not_equal" = queryOperator(`!=`),
  "in" = queryOperator(`%in%`),
  "not_in" = queryOperator(!`%in%`),
  "less" = queryOperator(`<`),
  "less_or_equal" = queryOperator(`<=`),
  "greater" = queryOperator(`>`),
  "greater_or_equal" = queryOperator(`>=`),
  "between" = queryOperator(queryBuilder::in_range),
  "not_between" = queryOperator(!queryBuilder::in_range),
  "begins_with" = queryOperator(startsWith),
  "not_begins_with" = queryOperator(!startsWith),
  "contains" = queryOperator(queryBuilder::in_string),
  "not_contains" = queryOperator(!queryBuilder::in_string),
  "ends_with" = queryOperator(endsWith),
  "not_ends_with" = queryOperator(!endsWith),
  "is_empty" = queryOperator(queryBuilder::is_empty),
  "not_is_empty" = queryOperator(!queryBuilder::is_empty),
  "is_null" = queryOperator(is.na),
  "not_is_null" = queryOperator(!is.na)
)

#' @rdname query-condition
#' @export
default_conditions <- list(
  "AND" = queryCondition(`&`),
  "OR" = queryCondition(`|`)
)

#' R6 class representing 'queryBuilderConfig' object.
#'
#' The object is responsible for storing definitions for operators and conditions
#' that are used to generate query expression.
#' It also allows to manage its objects by the provided methods.
#'
#' @param conditions Conditions.
#' @param operators Operators.
#' @return R6 Class constructor for query configuration (operators, conditions and methods for managing the objects).
#' @export
queryBuilderConfigClass <- R6::R6Class(
  "queryBuilderConfig",
  lock_objects = FALSE,
  public = list(
    #' @description
    #' Create queryBuilderConfig object with initialized conditions and operators.
    #' @param ... Unused.
    #' @return The object of class `queryBuilderConfig`.
    initialize = function(conditions = default_conditions, operators = default_operators, ...) {
      private$conditions <- conditions
      private$operators <- operators
    },
    #' @description
    #' Add conditions and conditions to 'queryBuilderConfig' object.
    add = function(conditions = NULL, operators = NULL) {
      if (!is.null(operators)) {
        private$operators <- attach_to_list(private$operators, operators)
      }
      if (!is.null(conditions)) {
        private$conditions <- attach_to_list(private$conditions, conditions)
      }
    },
    #' @description
    #' Remove conditions or operators from 'queryBuilderConfig' object.
    #' @param conditions_id Id of conditions to remove.
    #' @param operators_id Id of operators to remove.
    remove = function(conditions_id = NULL, operators_id = NULL) {
      private$operators <- remove_by_name(private$operators, operators_id)
      private$conditions <- remove_by_name(private$conditions, conditions_id)
    },
    #' @description
    #' Get private elements from 'queryBuilderConfig' object.
    #' @param name Name of the element to get.
    get_from_private = function(name) {
      if (missing(name)) {
        return(private)
      }
      return(private[[name]])
    },
    #' @description
    #' Set private elements to 'queryBuilderConfig' object.
    #' @param name Name of the element to set.
    #' @param value New element value.
    set_to_private = function(name, value) {
      private[[name]] <- value
      return(invisible(NULL))
    },
    #' @description
    #' Restore default conditions and conditions of 'queryBuilderConfig' object and clear out remaining private objects.
    reset = function() {
      private$conditions <- default_conditions
      private$operators <- default_operators
      extra_objects <- setdiff(names(private), c("operators", "conditions"))
      for (obj in extra_objects) {
        private[[obj]] <- NULL
      }
      return(invisible(NULL))
    }
  ),
  private = list(
    conditions = NULL,
    operators = NULL
  )
)

#' Default object storing 'queryBuilder' configuration.
#'
#' @export
queryBuilderConfig <- queryBuilderConfigClass$new()
