

#' @export
get_sample = function(object, ...) UseMethod("get_sample")


#' @export
get_error = function(object, ...) UseMethod("get_error")


#' @export
get_warning = function(object, ...) UseMethod("get_warning")


#' @export
get_message = function(object, ...) UseMethod("get_message")


#' @export
get_condition = function(object, ...) UseMethod("get_condition")


has_error = function(object, ...) {
  e = get_error(object, ...)
  stopifnot(is_list(e))
  return(!is_empty(e))
}


has_warning = function(object, ...) {
  e = get_warning(object, ...)
  stopifnot(is_list(e))
  return(!is_empty(e))
}


#' @export
probability = function(object, ...) UseMethod("probability")
