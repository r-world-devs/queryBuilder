#' Substitute expression stored as a variable
#'
#' @param x Expression to be substituted.
#' @param env List of arguments to substitute for x.
substitute_q <- function(x, env) {
  call <- substitute(substitute(y, env), list(y = x))
  eval(call)
}

#' Extract attribute of each element from a set of lists
#'
#' @param list_obj List of lists. Each nested list should contain \code{el_name} object.
#' @param attribute Name of the attribute to extract from each object.
#'
#' @return Vector of the same length, storing extracted attributes.
lget_attr <- function(list_obj, attribute) {
  purrr::map_vec(list_obj, ~class(.x))
}

#' Generate error message
#'
#' @param msg Character string interpreted by \link[glue]{glue}.
#' @param ... Extra arguments passed to \link[glue]{glue}.
#'
#' @return Executed error with interpolated message.
err_msg <- function(msg, ...) {
  stop(glue::glue(msg, ...), call. = FALSE)
}

#' Combine two lists
#'
#' @param base_list List to attach objects to.
#' @param extra_list List from which elements should be attached to \code{base_list}.
#'   Duplicated objects are overwritten.
#'
#' @return List.
attach_to_list <- function(base_list, extra_list) {
  modified_list <- utils::modifyList(
    base_list,
    extra_list
  )
  return(modified_list)
}

#' Remove list elements by their names
#'
#' @param list_obj List object.
#' @param ids Objects names to be removed.
#'
#' @return List.
remove_by_name <- function(list_obj, ids) {
  list_obj[ids] <- NULL
  return(list_obj)
}
