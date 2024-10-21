

#' @export
get_data = function(object, ...) UseMethod("get_data")


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


#' @export
get_result = function(object, ...) UseMethod("get_result")


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


#' @export
plot_prior = function(object, ...) UseMethod("plot_prior")


#' @export
plot_prior_predictive = function(object, ...) UseMethod("plot_prior_predictive")


#' @export
plot_posterior = function(object, ...) UseMethod("plot_posterior")

