#' Build Filtering Query from Configuration
#'
#' @name queryBuilder-package
#' @importFrom magrittr %>%
#' @importFrom dplyr sym
NULL

.onLoad <- function(...) {
  queryBuilderConfig$reset()
}

.onUnload <- function(...) {
  queryBuilderConfig$reset()
}

globalVariables(c("."))
